(* open Lwt *)
(* open Lwt.Syntax *)

type config = {
  uri: string;
} [@@deriving show]
;;

type db = {
  config: config;
} [@@deriving show]
;;

let new_db config =
  {config}
;;

let load_partial name =
  try
    let filepath = "bin/queries/" ^ name ^ ".xml" in
    In_channel.with_open_text filepath In_channel.input_all
    |> Mustache.of_string
    |> (fun x -> Some x)
  with
    e -> let err_str = Printexc.to_string e in
      Logs.err (fun m -> m "Error while trying to load partial: %s" err_str);
      None
;;

let render_query template_name json_data =
  (* let template_file = open_in "queries/" ^ template_name ^ ".xquery" in *)
  let filepath = "bin/queries/" ^ template_name ^ ".xquery" in
  let template = In_channel.with_open_text filepath In_channel.input_all
  |> Mustache.of_string in
  Mustache.render template json_data ~partials:load_partial
;;

let process_response res =
  (* TODO *)
  res
;;

let submit_query (db: db) (query: string) =
  try%lwt
    let uri = Uri.of_string db.config.uri in
    let req_body = Cohttp_lwt.Body.of_string query in
    let%lwt (response, res_body) = Cohttp_lwt_unix.Client.post uri ~body:req_body in
    let code = response
      |> Cohttp.Response.status
      |> Cohttp.Code.code_of_status in
    if code >= 200 && code <= 299 then
      let%lwt response_body = res_body
        |> Cohttp_lwt.Body.to_string in
      Lwt.return @@ Ok response_body
    else
      Lwt.return @@ Error (code, "HTTP unsuccessful code")
  with
    err -> let err_str = Printexc.to_string err in
      Logs.err (fun m -> m "error: submit_query %s" err_str);
      Lwt.return @@ Error (500, err_str)
;;

(*
let test_mustache () =
  (* let header = open_in "queries/header.xml" in *)
  (* let footer = open_in "queries/footer.xml" in *)
  (* let query = open_in "queries/row_count.xquery" in *)
  (* let partials _ = *)
    (* Some(Mustache.of_string "justafoo") in *)
  (* let template = Mustache.of_string "={{name}}={{>foo}}=" in *)
  let json = `O ["name", `String "Ocaml"] in
  let rendered = render_query "query_count" json in
  Printf.printf "thebody: %s\n%!" rendered;
  rendered
;;
*)

let query_row_count (db: db) =
  Logs.debug (fun m -> m "start: query_row_count %s" (show_db db));
  let json = `O ["name", `String "Ocaml"] in
  let query = render_query "query_count" json in
  let%lwt result = submit_query db query in
  Logs.err (fun m -> m "tttttttttttttttttttttttttttttttttttttttttt");
  match result with
  | Ok result -> Logs.debug (fun m -> m "end: query_row_count %s %s" (show_db db) result); Lwt.return "ok"
  | Error (code, reason) -> Logs.err (fun m -> m "error: query_row_count %d %s" code reason); Lwt.return "error"

(*
let query_row_count (db: db) =
  Logs.debug (fun m -> m "start: query_row_count %s\n%!" (show_db db));
  let uri = Uri.of_string "http://localhost" in
  let body = Cohttp_lwt.Body.of_string("foo") in
  let%lwt (response, response_body) = Cohttp_lwt_unix.Client.post uri ~body in
  let code = response
    |> Cohttp.Response.status
    |> Cohttp.Code.code_of_status in
  let%lwt response_body_str = response_body
    |> Cohttp_lwt.Body.to_string in
  Logs.debug (fun m -> m "end: query_row_count %s %d %s\n%!" (show_db db) code response_body_str);
  Lwt.return (response, code, response_body_str)
;;
*)

(*
let query_row_count1 (d: db) =
  Printf.printf "db: %s\n%!" (show_db d);
  let uri = Uri.of_string "https://www.google.de" in
  let* (resp, body) = Cohttp_lwt_unix.Client.get uri in
  let code = resp
    |> Cohttp.Response.status
    |> Cohttp.Code.code_of_status in
  let* body_str = body
    |> Cohttp_lwt.Body.to_string in
  Printf.printf "code: %d, bodylen: %d\n%!" code (String.length body_str);
  Lwt.return (code, body_str)
;;

let query_row_count2 (d: db) =
  Printf.printf "db: %s\n%!" (show_db d);
  let uri = Uri.of_string "https://www.google.de" in
  let%lwt (resp, body) = Cohttp_lwt_unix.Client.get uri in
  let code = resp
    |> Cohttp.Response.status
    |> Cohttp.Code.code_of_status in
  let%lwt body_str = body
    |> Cohttp_lwt.Body.to_string in
  Printf.printf "code: %d, bodylen: %d\n%!" code (String.length body_str);
  Lwt.return (code, body_str)
;;

let query_row_count3 (d: db) =
  Printf.printf "db: %s\n%!" (show_db d);
  let uri = Uri.of_string "https://www.google.de" in
  Lwt.bind (Cohttp_lwt_unix.Client.get uri) (fun (resp, body) ->
    Lwt.map (fun (body_str) ->
      let code = resp
        |> Cohttp.Response.status
        |> Cohttp.Code.code_of_status in
      (code, body_str)) (Cohttp_lwt.Body.to_string body))
;;

let query_row_count4 (d: db) =
  Printf.printf "db: %s\n%!" (show_db d);
  let uri = Uri.of_string "https://www.google.de" in
  Cohttp_lwt_unix.Client.get uri >>= fun (resp, body) ->
    Cohttp_lwt.Body.to_string body >|= fun (body_str) ->
      let code = resp
        |> Cohttp.Response.status
        |> Cohttp.Code.code_of_status in
      (code, body_str)
  (*
  Cohttp_lwt_unix.Client.get (Uri.of_string "https://www.google.de") >>= fun (resp, body) ->
  let code = resp |> Cohttp.Response.status |> Cohttp.Code.code_of_status in
  Printf.printf "Response code: %d\n%!" code;
  Printf.printf "Headers: %s\n%!" (resp |> Cohttp.Response.headers |> Cohttp.Header.to_string);
  body |> Cohttp_lwt.Body.to_string >|= fun body ->
  Printf.printf "Body of length: %d\n%!" (String.length body);
  body
  *)
;;
*)

