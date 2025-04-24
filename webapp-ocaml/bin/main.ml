let successful = ref 0
let failed = ref 0

let field1 = Dream.new_field ~name:"field1" ()

let count_requests inner_handler request =
  try%lwt
    let%lwt response = inner_handler request in
    successful := !successful + 1;
    Lwt.return response

  with exn ->
    failed := !failed + 1;
    raise exn
;;

let make_authenticated header_name inner_handler request ~header =
  let info = Dream.header request header in
  match info with
  | None -> "";
  | Some x -> x;
  |> Dream.set_field request field1;

  let user = Dream.header request header_name in
  match user with
  | None -> print_endline "None"; request;
  | Some user -> Printf.printf "User: %s\n%!" user; request;
  |> inner_handler
;;

type cli_args = {
  authuser_header: string option;
  static_authuser: string option;
} [@@deriving show]

let read_cli_args () =
  let authuser_header = ref "" in
  let static_authuser = ref "" in
  let arglist = [
    ("-authuser-header", Arg.Set_string authuser_header, "HTTP header to fetch the authuser from");
    ("-static-authuser", Arg.Set_string static_authuser, "Set authuser to this value (useful for debugging/dev)");
  ] in
  Arg.parse arglist (fun _ -> ()) "help me";
  let au = match !authuser_header with
    | "" -> None
    | x -> Some x in
  let sa = match !static_authuser with
    | "" -> None
    | x -> Some x in
  {
    authuser_header = au;
    static_authuser = sa;
  }
;;

let () =
  let db = Existdb.new_db {
    host = "localhost";
    port = 1234;
    collection = "dmarc";
  } in
  Printf.printf "%s\n%!" (Existdb.show_db db);

  let args = read_cli_args () in
  Printf.printf "args: %s\n%!" (show_cli_args args);

  Dream.run ~port:8081
  @@ Dream.logger
  @@ make_authenticated "remote-user" ~header:"remote-groups"
  @@ count_requests
  @@ Dream.router [

    Dream.get "/fail"
      (fun _ ->
        raise (Failure "The web app failed!"));

    Dream.get "/"
      (fun request ->
        let field1val = match Dream.field request field1 with
          | None -> "nope"
          | Some x -> x
        in Dream.html (Printf.sprintf "successful: %3i, failed: %3i, header: %s" !successful !failed field1val));

    Dream.get "/echo/:word"
      (fun request ->
        Dream.html (Dream.param request "word"));

    Dream.get "/headers"
      (fun request ->
        Dream.html ([%derive.show: (string * string) list](Dream.all_headers request)));

    Dream.get "/request"
      (fun request ->
        Dream.log "foobar %s" (Dream.client request);
        Dream.html "dings");

    Dream.get "/query"
      (fun _ ->
        View.query "themainpage"
        |> Dream.html);

  ]
;;

