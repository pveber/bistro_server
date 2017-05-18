open Sexplib.Std
open Js_browser
open Bistro_server_common
module List = CCListLabels
module Option = CCOpt

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

let form ?a xs = Vdom.elt "form" ?a xs

let legend l = Vdom.(elt "legend" [ text l ])
let legend_ = legend

let fieldset ?(a = []) ?legend xs =
  let open Vdom in
  Vdom.elt "fieldset" ~a @@ List.cons_maybe (Option.map legend_ legend) xs

let br () = Vdom.elt "br" []

let label ?a xs = Vdom.elt ?a "label" xs

let rec form_view ?legend { fields } =
  form [ fieldset ?legend @@ List.flat_map fields ~f:field_view ]

and field_view =
  let open Vdom in
  function
  | lab, Int_field value -> [
      label [text lab] ;
      input
        ~a:[attr "type" "number"]
        Option.(to_list @@ map text @@ map string_of_int value) ;
      br () ;
    ]
  | lab, String_field value -> [
      label [text lab] ;
      input Option.(to_list @@ map text value) ;
      br () ;
    ]
  | lab, Form_field f -> [
      form_view ~legend:lab f ; br ()
    ]

let update m () =
  m, Vdom.Cmd.Batch []

let view spec =
  let open Vdom in
  div ~a:[attr "class" "container"] [
    text spec.app_title ;
    form_view spec.app_form ;
  ]

let main spec =
  let init = spec, Vdom.Cmd.Batch [] in
  let app = Vdom.app ~init ~update ~view () in
  Vdom_blit.run app
  |> Vdom_blit.dom
  |> Element.append_child (Document.body document)

let onload _ =
  Lwt.async (fun () ->
      http_request ["app_specification"]
      >>| Sexplib.Sexp.of_string
      >>| app_specification_of_sexp
      >>| main
    )

let _ =
  Window.set_onload window onload
