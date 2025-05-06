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

type my_dom =
  | Text of string
  | Element of string * my_dom list
  [@@deriving show]

let process_response xml_str =
  let _result = xml_str
  |> Markup.string
  |> Markup.parse_xml
  |> Markup.signals
  (* |> Markup.map (fun x -> match x with | `Start_element (_n, _) -> "el" | _ -> "o") *)
  (* |> Markup.filter (function `Text _ -> true | `Start_element _ -> true | _ -> false) *)
  |> Markup.tree
    ~text:(fun ss -> Printf.printf "TEXT %d %s\n%!" (List.length ss) ([%derive.show: string list]ss); Text (String.concat " " ss))
    ~element:(fun (xa, xb) ys zs -> Printf.printf "ELEMENT %s:%s %d:%s %d:%s\n%!"
        xa xb
        (List.length ys)
        (* ([%derive.show: (Markup.name * string) list]ys) *)
        (String.concat " " @@ List.map (fun ((na, nb), s) -> Printf.sprintf "%s/%s:%s" na nb s) ys)
        (List.length zs)
        ([%derive.show: my_dom list]zs);
        Element (xa, zs)
        (* match xb with *)
        (* | "item" -> Some { *)
        (*   qbegin = 123; *)
        (*   qend = 234; *)
        (*   rowcount = 345; *)
        (* } *)
        (*   | _ -> None *)
      ) in
  (* |> Markup.pretty_print *)
  (* |> Markup.write_xml *)
  (* |> Markup.to_string *)
  "foo"
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

let query_row_count (db: db) range_begin range_end =
  Logs.debug (fun m -> m "start: query_row_count %s" (show_db db));
  (* let%lwt _ = Lwt_unix.sleep 5. in *)
  let json =
    `O ["variables",
      `A [
        `O [
          "key", `String "wantedBegin";
          "type", `String "integer";
          (* "value", `Float 1715689600. *)
          "value", `Float range_begin];
        `O [
          "key", `String "wantedEnd";
          "type", `String "integer";
          (* "value", `Float 1742974400. *)
          "value", `Float range_end]]] in
  let query = render_query "query_count" json in
  let%lwt result = submit_query db query in
  match result with
  | Ok result ->
    Logs.debug (fun m -> m "end: query_row_count %s %s" (show_db db) result);
    Lwt.return @@ Ok(process_response result)
  | Error (code, reason) ->
    Logs.err (fun m -> m "error: query_row_count %d %s" code reason);
    Lwt.return @@ Error reason

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

