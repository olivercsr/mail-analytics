let successful = ref 0
let failed = ref 0

let count_requests inner_handler request =
  try%lwt
    let%lwt response = inner_handler request in
    successful := !successful + 1;
    Lwt.return response

  with exn ->
    failed := !failed + 1;
    raise exn

let () =
  Dream.run
  @@ Dream.logger
  @@ count_requests
  @@ Dream.router [

    Dream.get "/fail"
      (fun _ ->
        raise (Failure "The web app failed!"));

    Dream.get "/"
      (fun _ ->
        Dream.html (Printf.sprintf "successful: %3i, failed: %3i" !successful !failed));

    Dream.get "/echo/:word"
      (fun request ->
        Dream.html (Dream.param request "word"));

    Dream.get "/headers"
      (fun request ->
        Dream.html ([%derive.show: (string * string) list](Dream.all_headers request)));

  ]

