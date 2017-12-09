open Sexplib.Std
open Js_browser
open Bistro_server_common
module List = CCListLabels
module Option = CCOpt

let ( % ) f g x = g (f x)
let ( >>= ) = Lwt.( >>= )
let ( >>| ) = Lwt.( >|= )

let read_file file =
  let reader = FileReader.new_file_reader () in
  let wait, notify = Lwt.wait () in
  FileReader.read_as_text reader file ;
  FileReader.set_onload reader (fun () ->
      Lwt.wakeup notify (FileReader.result reader)
    ) ;
  wait

type 'a Vdom.Cmd.t +=
  | Http_request : {
      meth : [ `GET | `POST ] ;
      path : string list ;
      body : string Lwt.t ;
      handler : int -> string -> 'a
    } -> 'a Vdom.Cmd.t

module String_map = Map.Make(String)

type model =
  | Form of {
      title : string ;
      form : form ;
      selected_files : File.t String_map.t ;
    }
  | Data_upload of {
      title : string ;
      run_id : string ;
      files : (string * File.t * [`TODO | `INPROGRESS | `DONE | `FAILED]) list ;
    }

let string_of_upload_status = function
  | `TODO -> "TODO"
  | `INPROGRESS -> "INPROGRESS"
  | `DONE -> "DONE"
  | `FAILED -> "FAILED"


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
      | XHR.Done -> Lwt.wakeup wakener (XHR.status xhr, XHR.response_text xhr)
      | _ -> ()
    ) ;
  XHR.open_ xhr (string_of_meth meth) uri ;
  XHR.send xhr body ;
  waiter

let button ?(a = []) msg label =
  Vdom.elt "button" ~a:(Vdom.onclick msg :: a) [ Vdom.text label ]

let h2 ?a xs = Vdom.elt ?a "h2" xs

let form ?a xs = Vdom.elt "form" ?a xs

let legend l = Vdom.(elt "legend" [ text l ])
let legend_ = legend

let fieldset ?(a = []) ?legend xs =
  let open Vdom in
  Vdom.elt "fieldset" ~a @@ List.cons_maybe (Option.map legend_ legend) xs

let br () = Vdom.elt "br" []

let label ?a xs = Vdom.elt ?a "label" xs

let table ?a xs = Vdom.elt ?a "table" xs

let tr ?a xs = Vdom.elt ?a "tr" xs

let td ?a xs = Vdom.elt ?a "td" xs

let rec assoc_replace xs k v =
  match xs with
  | [] -> assert false
  | ((k', _) as p) :: t ->
    if k = k' then (k, v) :: t
    else p :: assoc_replace t k v

let input_id_of_path p =
  String.concat "." (List.rev p)

let rec form_view_aux ?(root = false) ?legend form_path { fields } =
  let f (label, field_kind) =
    field_view (label :: form_path) label field_kind
  in
  form Vdom.[
    fieldset ?legend @@ List.map fields ~f ;
    if root then
      div []
    else
      div []
  ]

and field_view field_path lab field =
  let open Vdom in
  div ~a:[attr "class" "form-group"] (
    field_view_aux field_path lab field
  )

and field_view_aux field_path lab =
  let open Vdom in
  let id = input_id_of_path field_path in
  function
  | Int_field value -> [
      label ~a:[attr "for" id] [text lab] ;
      (
        let a =
          List.cons_maybe
            Option.(map (string_of_int % attr "value") value)
            [
              attr "class" "form-control" ;
              attr "id" id ;
              attr "type" "number" ;
              oninput (fun _ -> `Form_update (field_path, `Other)) ;
            ]
        in
        input ~a []
      ) ;
    ]
  | String_field value -> [
      label ~a:[attr "for" id] [text lab] ;
      (
        let a =
          List.cons_maybe
            Option.(map (attr "value") value)
            [
              attr "class" "form-control" ;
              attr "id" id ;
              oninput (fun _ -> `Form_update (field_path, `Other)) ;
            ]
        in
        input ~a []
      ) ;
    ]
  | File_field value -> [
      label ~a:[attr "for" id] [text lab] ;
      (
        let a =
          List.cons_maybe
            Option.(map (attr "value") value)
            [
              attr "class" "form-control" ;
              attr "id" id ;
              attr "type" "file" ;
              onchange (fun _ -> `Form_update (field_path, `File))
            ]
        in
        input ~a []
      ) ;
    ]
  | Form_field f ->
    [ form_view_aux ~legend:lab field_path f ]

let form_view spec =
  form_view_aux ~root:true [] spec

let get_elt_exn id =
    match Document.get_element_by_id (Window.document window) id with
    | None -> assert false
    | Some elt -> elt

let rec update_form { fields } id = function
  | [] -> assert false
  | h :: t ->
    let f ((label, field) as p) =
      if label = h then (label, update_field field id t)
      else p
    in
    { fields = List.map ~f fields }

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
    File_field (Some id)

  | Form_field form ->
    Form_field (update_form form id path)

let update_selected_files selected_files id =
  let input = get_elt_exn id in
  match Element.files input with
  | [] -> String_map.remove id selected_files
  | file :: _ -> String_map.add id file selected_files

let upload_file ~run_id ~file_id file =
  Http_request {
    meth = `POST ;
    path = ["upload" ; run_id ; file_id] ;
    body = read_file file ;
    handler = fun code _ ->
      match code with
      | 200 -> `Upload_completed (run_id, file_id)
      | _ -> `Upload_failed (run_id, file_id)
  }

let update_file_upload_status files file_id status =
  List.map files ~f:(fun ((id, file, _) as e) ->
      if id = file_id then (id, file, status)
      else e
    )

let rec update m msg =
  match m, msg with
  | Form f, `Form_update (path, ty) ->
    let id = input_id_of_path path in
    let form = update_form f.form id (List.rev path) in
    let selected_files = match ty with
      | `File -> update_selected_files f.selected_files id
      | `Other -> f.selected_files
    in
    Vdom.return @@ Form { f with form ; selected_files }

  | Form form, `Run -> (
      match form_value form.form with
      | Some value_sexp ->
        let input_files = form_files form.form in
        let input_file_descrs =
          List.map input_files ~f:(fun fn_id ->
              { input_file_id = fn_id ; input_file_md5 = "" }
            )
        in
        let sexp = sexp_of_run_request CCFun.id {
            input = value_sexp ;
            files = input_file_descrs ;
          }
        in
        let c = [
          Http_request {
            meth = `POST ;
            path = ["run"] ;
            body = Lwt.return @@ Sexplib.Sexp.to_string sexp ;
            handler = fun _ run_id -> (* FIXME: check for error *)
              match input_files with
              | [] -> `Goto_url ["run" ; run_id]
              | h :: t ->
                let f fn =
                  fn, String_map.find fn form.selected_files, `TODO
                in
                let (fn1, file1, _) = f h in
                let state = Data_upload {
                  title = form.title ;
                  run_id ;
                  files = (fn1, file1, `INPROGRESS) :: List.map t ~f ;
                }
                in
                `Next (state, [ upload_file ~run_id ~file_id:fn1 file1 ])
          }
        ]
        in
        Vdom.return ~c m
      | None -> assert false (* FIXME *)
    )

  | Data_upload up, `Upload_completed (_, completed_file_id) -> (
      let files = update_file_upload_status up.files completed_file_id `DONE in
      match List.find files ~f:(fun (_, _, status) -> status = `TODO) with
      | (file_id, file, _) ->
        let files = update_file_upload_status files file_id `INPROGRESS in
        let m = Data_upload { up with files } in
        let c = [ upload_file ~run_id:up.run_id ~file_id file ] in
        Vdom.return ~c m
      | exception Not_found ->
        update (Data_upload { up with files })  (`Goto_url ["run" ; up.run_id])
    )

  | Data_upload up, `Upload_failed (_, completed_file_id) -> (
      let files = update_file_upload_status up.files completed_file_id `FAILED in
      Vdom.return (Data_upload { up with files }) (* FIXME: try other uploads? *)
    )
  | _, `Next (state, c) -> Vdom.return ~c state
  | _, `Goto_url path ->
    Location.assign (Window.location window) (site_uri path) ;
    Vdom.return m

  | Data_upload _, (`Form_update _ | `Run) -> assert false
  | Form _, (`Upload_completed _ | `Upload_failed _) -> assert false

let view m =
  let open Vdom in
  let contents = match m with
    | Form m -> [
        h2 [ text m.title ] ;
        br () ;
        form_view m.form ;
      button ~a:[attr "class" "btn btn-primary"] `Run "Run"
        
        (* FIXME: debugging stuff *)
        (* br () ; *)
        (* text @@ Sexplib.Sexp.to_string_hum @@ sexp_of_form m.form ; *)
        (* br () ; *)
        (* text @@ Sexplib.Sexp.to_string_hum @@ sexp_of_option CCFun.id @@ form_value m.form ; *)
        (* div @@ List.flat_map (String_map.bindings m.selected_files) ~f:(fun (k, _) -> *)
        (*     [ text k ; br () ] *)
        (*   ) *)
      ]

    | Data_upload up -> [
        h2 [ text up.title ] ;
        br () ;
        table @@ List.map up.files ~f:(fun (_, file, status) ->
            tr [ td [ text (File.name file) ] ;
                 td [ text (string_of_upload_status status) ] ]
          )

      ]
  in
  div ~a:[attr "class" "container"] contents

let cmd_handler = {
  Vdom_blit.Cmd.f = fun ctx ->
    function
    | Http_request { meth ; path ; body ; handler } ->
      Lwt.async (fun () ->
          body >>= fun body ->
          http_request meth path body >>| fun (code, body) ->
          Vdom_blit.Cmd.send_msg ctx (handler code body)
        ) ;
      true
    | _ -> false
}

let main spec =
  let model = Form { form = spec.app_form ;
                     title = spec.app_title ;
                     selected_files = String_map.empty } in
  let init = model, Vdom.Cmd.Batch [] in
  let app = Vdom.app ~init ~update ~view () in
  let env = Vdom_blit.(cmd cmd_handler) in
  Vdom_blit.run ~env app
  |> Vdom_blit.dom
  |> Element.append_child (Document.body document)

let onload _ =
  Lwt.async (fun () ->
      http_request `GET ["app_specification"] ""
      >>| CCFun.compose snd Sexplib.Sexp.of_string
      >>| app_specification_of_sexp
      >>| main
    )

let _ =
  Window.set_onload window onload
