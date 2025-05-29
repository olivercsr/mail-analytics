(* open Base *)
(* open Core *)
(* open Core_thread *)
(* open Lwt *)
(* open Lwt.Syntax *)

let successful = ref 0
let failed = ref 0

let field1 = Dream.new_field ~name:"field1" ()

(* type fox = [ `DaFox | `DaFuq ] *)

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
  | Some user -> Logs.debug (fun m -> m "User: %s\n%!" user); request;
  |> inner_handler
;;

type app_config = {
  port: int;
  loglevel: string;
  existdb_uri: string;
  authuser_header: string;
  static_authuser: string option;
} [@@deriving show]
;;

let read_app_config () =
  (* let port = ref 8080 in *)
  (* let existdb_uri = ref "" in *)
  (* let authuser_header = ref "remote-user" in *)
  (* let static_authuser = ref "" in *)
  (* let arglist = [ *)
  (*   ("-port", Arg.Set_int port, "Port to listen on"); *)
  (*   ("-existdb-uri", Arg.Set_string existdb_uri, "URI to ExistDb"); *)
  (*   ("-authuser-header", Arg.Set_string authuser_header, "HTTP header to fetch the authuser from"); *)
  (*   ("-static-authuser", Arg.Set_string static_authuser, "Set authuser to this value (useful for debugging/dev)"); *)
  (* ] in *)
  (* Arg.parse arglist (fun _ -> ()) "help me"; *)
  (* if !existdb_uri = "" then ( *)
  (*   Printf.eprintf "Please provide -existdb-uri\n%!"; *)
  (*   exit 1; *)
  (* ); *)
  {
    port = (match (Sys.getenv_opt "PORT") with
      | None -> 8080
      | Some p -> int_of_string p);
    loglevel = (match (Sys.getenv_opt "LOGLEVEL") with
      | None -> "info"
      | Some l -> l);
    existdb_uri = (match (Sys.getenv_opt "EXISTDB_URI") with
      | None -> (
        Printf.eprintf "Please provide EXISTDB_URI.\n%!";
        exit 1
      )
      | Some u -> u);
    authuser_header = (match (Sys.getenv_opt "AUTHUSER_HEADER") with
      | None -> "remote-user"
      | Some h -> h);
    static_authuser = (match (Sys.getenv_opt "STATIC_AUTHUSER") with
      | None -> None
      | Some "" -> None
      | Some u -> Some u);
  }
;;

(*
let doit x  =
  let* () = Lwt_unix.sleep (float_of_int x) in
  Lwt.return (x + 1)
;;
*)

(*
let dorequest () =
  Cohttp_lwt_unix.Client.get (Uri.of_string "https://www.google.de") >>= fun (resp, body) ->
  let code = resp |> Cohttp.Response.status |> Cohttp.Code.code_of_status in
  Printf.printf "Response code: %d\n%!" code;
  Printf.printf "Headers: %s\n%!" (resp |> Cohttp.Response.headers |> Cohttp.Header.to_string);
  body |> Cohttp_lwt.Body.to_string >|= fun body ->
  Printf.printf "Body of length: %d\n%!" (String.length body);
  body
*)

let init_logging level =
  let loglevel = match Logs.level_of_string level with
    | Ok level_opt -> level_opt
    | Error _ -> Some Logs.Info in
  Logs.set_reporter_mutex ~lock:(fun () -> ()) ~unlock:(fun () -> ());
  Logs.set_reporter @@ Logs_fmt.reporter ();
  Logs.set_level loglevel;
  (* Logs.debug (fun m -> m "============== DEBUG%!"); *)
  (* Logs.info (fun m -> m "============== INFO%!"); *)
  (* Logs.warn (fun m -> m "============== WARN%!"); *)

  let dream_level = match level with
  | "debug" -> `Debug
  | "info" -> `Info
  | "warn" -> `Warning
  | "error" -> `Error
  | _ -> `Info in
  Dream.initialize_log ~backtraces:true ~async_exception_hook:true ~level:dream_level ~enable:true ();
;;

let _main loglevel =
  init_logging loglevel;
  Logs.debug (fun m -> m "application starting...");

  let app_config = read_app_config () in
  Printf.printf "app_config: %s\n%!" (show_app_config app_config);

  let db = Existdb.new_db {
    (* uri = "http://localhost:8080/exist/rest/dmarc"; *)
    uri = app_config.existdb_uri;
  } in
  Printf.printf "%s\n%!" (Existdb.show_db db);
  (*
  let p1 = doit 8 in
  let p2 = doit 3 in
  let r = Lwt_main.run @@ Lwt.pick [p1; p2;] in
  Printf.printf "res: %i\n%!" r;
  *)

  (*
  let body = Lwt_main.run (dorequest ()) in
  print_endline ("Received body" ^ body);
  *)

  Lwt.return
  @@ Dream.run ~port:app_config.port
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

    Dream.get "/wait"
      (fun _ ->
        let%lwt _ = Lwt_unix.sleep 5. in
        (* Core_thread.delay 5.; *)
        Dream.html "waited");

    Dream.get "/query/count/:start/:end"
      (fun request ->
        (* Query.query "themainpage" *)
        (* |> Dream.html *)
        (* Existdb.test_mustache () |> Dream.html *)
        (* let%lwt (Ok res|Error res) = Existdb.query_row_count db in *)
        (* Logs.set_level (Some Logs.Debug); *)
        (* let day = 60. *. 60. *. 24. *)
        (* and now = Unix.time () in *)
        (* let range_begin = now -. day *. 30. *)
        (* and range_end = now *)
        let range_begin = Dream.param request "start" |> float_of_string
        and range_end = Dream.param request "end" |> float_of_string in
        let%lwt result = Existdb.query_row_count db range_begin range_end in
        match result with
        | Ok res -> Dream.html res
        | Error err -> Dream.html @@ "ERROR" ^ err
      );

  ]
;;

let _task n =
  Printf.printf "[%d] task start\n%!" n;
  let%lwt _ = Lwt_unix.sleep 10. in
  Printf.printf "[%d] task end\n%!" n;
  Lwt.return ()
;;

let () =
  (* Lwt_main.run @@ Lwt.join [task (); task ()] *)
  (* Lwt_main.run @@ Lwt.join @@ List.init 100_0000 task; *)
  (* Lwt_main.run @@ task () *)
  Lwt_main.run @@ _main "debug"
;;

