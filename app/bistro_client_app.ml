open Sexplib.Std
open Js_browser
open Bistro_server_common

let ( >>= ) = Lwt.( >>= )
let ( >>| ) = Lwt.( >|= )

let http_request path =
  let waiter, wakener = Lwt.wait () in
  let uri = String.concat "" [
      "http://" ;
      window |> Window.location |> Location.host ;
      "/" ;
      String.concat "/" path ;
    ]
  in
  let xhr = XHR.create () in
  XHR.set_onreadystatechange xhr (fun () ->
      match XHR.ready_state xhr with
      | XHR.Done -> Lwt.wakeup wakener (XHR.response_text xhr)
      | _ -> ()
    ) ;
  XHR.open_ xhr "GET" uri ;
  XHR.send xhr "" ;
  waiter

let start _ =
  Lwt.async (fun () ->
      http_request ["app_specification"]
      >>| Sexplib.Sexp.of_string
      >>| app_specification_of_sexp
      >>| fun spec ->
      Window.alert window spec.app_title
    )

let _ =
  Window.set_onload window start
