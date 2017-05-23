open Sexplib.Std
open Js_browser
open Bistro_server_common
module List = CCListLabels
module Option = CCOpt

let ( % ) f g x = g (f x)
let ( >>= ) = Lwt.( >>= )
let ( >>| ) = Lwt.( >|= )


type 'a Vdom.Cmd.t +=
  | Http_request : {
      meth : [ `GET | `POST ] ;
      path : string list ;
      body : string ;
      handler : string -> 'a
    } -> 'a Vdom.Cmd.t

module String_map = Map.Make(String)

type model =
  | Application_form of {
      form : form ;
      selected_files : File.t String_map.t
    }

let string_of_meth = function
  | `GET -> "GET"
  | `POST -> "POST"

let site_uri path =
  String.concat "" [
    "http://" ;
    window |> Window.location |> Location.host ;
    "/" ;
    String.concat "/" path ;
  ]

let http_request meth path body =
  let waiter, wakener = Lwt.wait () in
  let uri = site_uri path in
  let xhr = XHR.create () in
  XHR.set_onreadystatechange xhr (fun () ->
      match XHR.ready_state xhr with
      | XHR.Done -> Lwt.wakeup wakener (XHR.response_text xhr)
      | _ -> ()
    ) ;
  XHR.open_ xhr (string_of_meth meth) uri ;
  XHR.send xhr body ;
  waiter

let button msg label =
  Vdom.elt "button" ~a:[Vdom.onclick msg] [ Vdom.text label ]

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

let input_id_of_path p =
  String.concat "." (List.rev p)

let rec form_view_aux ?legend form_path { fields } =
  let f (label, field_kind) =
    field_view (label :: form_path) label field_kind
  in
  form [ fieldset ?legend @@ List.flat_map fields ~f ]

and field_view field_path lab =
  let open Vdom in
  let id = input_id_of_path field_path in
  function
  | Int_field value -> [
      label [text lab] ;
      (
        let a =
          List.cons_maybe
            Option.(map (string_of_int % attr "value") value)
            [
              attr "id" id ;
              attr "type" "number" ;
              oninput (fun _ -> `Form_update field_path) ;
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
              attr "id" id ;
              oninput (fun _ -> `Form_update field_path) ;
            ]
        in
        input ~a []
      ) ;
      br () ;
    ]
  | File_field value -> [
      label [text lab] ;
      (
        let a =
          List.cons_maybe
            Option.(map (attr "value") value)
            [
              attr "id" id ;
              attr "type" "file" ;
              onchange (fun _ -> `Form_update field_path)
            ]
        in
        input ~a []
      ) ;
      br () ;
    ]
  | Form_field f ->
    [ form_view_aux ~legend:lab field_path f ; br () ]

let form_view spec =
  form_view_aux [] spec

let get_elt_exn id =
    match Document.get_element_by_id (Window.document window) id with
    | None -> assert false
    | Some elt -> elt

let rec update_form { fields } id = function
  | [] -> assert false
  | h :: t ->
    {
      fields =
        List.map fields ~f:(fun ((label, field) as p) ->
            if label = h then (
              label, update_field field id t
            )
            else p
          )
    }

and update_field field id path =
  let input = get_elt_exn id in
  match field with
  | String_field _ ->
    let value = Element.value input in
    String_field (Some value)

  | Int_field _ ->
    let value = Element.value input in
    Int_field (Some (int_of_string value))

  | File_field _ ->
    let value = Element.value input in
    File_field (Some value)

  | Form_field form ->
    Form_field (update_form form id path)

let update m = function
  | `Form_update path ->
    let id = input_id_of_path path in
    Vdom.return { m with app_form = update_form m.app_form id (List.rev path) }

  | `Run -> (
      match form_value m.app_form with
      | Some sexp ->
        let c = [
          Http_request {
            meth = `POST ;
            path = ["run"] ;
            body = Sexplib.Sexp.to_string sexp ;
            handler = fun s -> `Goto [ "run" ; s ]
          }
        ]
        in
        Vdom.return ~c m
      | None -> assert false (* FIXME *)
    )

  | `Goto path ->
    Location.assign (Window.location window) (site_uri path) ;
    Vdom.return m

let view spec =
  let open Vdom in
  div ~a:[attr "class" "container"] [
    h2 [ text spec.app_title ] ;
    br () ;
    form_view spec.app_form ;
    button `Run "Run" ;
    br () ;
    text @@ Sexplib.Sexp.to_string_hum @@ sexp_of_form spec.app_form ;
    br () ;
    text @@ Sexplib.Sexp.to_string_hum @@ sexp_of_option CCFun.id @@ form_value spec.app_form ;
  ]

let cmd_handler = {
  Vdom_blit.Cmd.f = fun ctx ->
    function
    | Http_request { meth ; path ; body ; handler } ->
      Lwt.async (fun () ->
          http_request meth path body >>| fun body ->
          Vdom_blit.Cmd.send_msg ctx (handler body)
        ) ;
      true
    | _ -> false
}

let main spec =
  let init = spec, Vdom.Cmd.Batch [] in
  let app = Vdom.app ~init ~update ~view () in
  let env = Vdom_blit.(cmd cmd_handler) in
  Vdom_blit.run ~env app
  |> Vdom_blit.dom
  |> Element.append_child (Document.body document)

let onload _ =
  Lwt.async (fun () ->
      http_request `GET ["app_specification"] ""
      >>| Sexplib.Sexp.of_string
      >>| app_specification_of_sexp
      >>| main
    )

let _ =
  Window.set_onload window onload
