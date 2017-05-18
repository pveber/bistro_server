open Sexplib.Std
open Js_browser
open Bistro_server_common
module List = CCListLabels
module Option = CCOpt

let ( % ) f g x = g (f x)
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

let h2 ?a xs = Vdom.elt ?a "h2" xs

let form ?a xs = Vdom.elt "form" ?a xs

let legend l = Vdom.(elt "legend" [ text l ])
let legend_ = legend

let fieldset ?(a = []) ?legend xs =
  let open Vdom in
  Vdom.elt "fieldset" ~a @@ List.cons_maybe (Option.map legend_ legend) xs

let br () = Vdom.elt "br" []

let label ?a xs = Vdom.elt ?a "label" xs

let rec assoc_replace xs k v =
  match xs with
  | [] -> assert false
  | ((k', _) as p) :: t ->
    if k = k' then (k, v) :: t
    else p :: assoc_replace t k v

let rec form_view ?legend k { fields } =
  let f (label, field_kind) =
    let k x = k { fields = assoc_replace fields label x } in
    field_view k label field_kind
  in
  form [ fieldset ?legend @@ List.flat_map fields ~f ]

and field_view k lab =
  let open Vdom in
  function
  | Int_field value -> [
      label [text lab] ;
      (
        let a =
          List.cons_maybe
            Option.(map (string_of_int % attr "value") value)
            [
              attr "type" "number" ;
              oninput (fun i -> k (Int_field (Some (int_of_string i)))) ;
            ]
        in
        input ~a []
      ) ;
      br () ;
    ]
  | String_field value -> [
      label [text lab] ;
      (
        let a =
          List.cons_maybe
            Option.(map (attr "value") value)
            [
              oninput (fun s -> k (String_field (Some s))) ;
            ]
        in
        input ~a []
      ) ;
      br () ;
    ]
  | Form_field f ->
    let k x = k (Form_field x) in
    [ form_view ~legend:lab k f ; br () ]

let update m app_form =
  { m with app_form }, Vdom.Cmd.Batch []

let view spec =
  let open Vdom in
  div ~a:[attr "class" "container"] [
    h2 [ text spec.app_title ] ;
    br () ;
    form_view (fun x -> x) spec.app_form ;
    text @@ Sexplib.Sexp.to_string_hum @@ sexp_of_form spec.app_form ;
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
