open Lwt
open Lwt.Syntax

type config = {
  host: string;
  port: int;
  collection: string;
} [@@deriving show]
;;

type db = {
  config: config;
} [@@deriving show]
;;

let new_db config =
  {config}
;;

let query_row_count (db: db) =
  Logs.debug (fun m -> m "start: query_row_count %s\n%!" (show_db db));
  Logs.debug (fun m -> m "end: query_row_count %s\n%!" (show_db db))
;;

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

