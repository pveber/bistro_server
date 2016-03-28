[%%shared
    open Eliom_lib
    open Eliom_content
    open Html5.D
]

open Core.Std
open Bistro_engine

let db = Db.init_exn "_bistro"

module Bistro_server_app =
  Eliom_registration.App (
    struct
      let application_name = "bistro_server"
    end)

let main_service =
  Eliom_service.App.service ~path:[] ~get_params:Eliom_parameter.unit ()


(* REST API *)
module API = struct
  open Eliom_service.Http

  let send_string ~code body =
    Eliom_registration.String.send ~code (body, "text/plain")

  let read_raw_content ?(length = 4096) raw_content =
    let content_stream = Ocsigen_stream.get raw_content in
    Ocsigen_stream.string_of_stream length content_stream

  module Wave = struct
    let path = [ "api" ; "wave" ]
    let get_params = Eliom_parameter.(suffix (neopt (string "name")))

    let get = service ~path ~get_params ()
    let post = post_service ~fallback:get ~post_params:Eliom_parameter.raw_post_data ()

    let get_handler name_opt () =
      match name_opt with
      | None ->
        Db.Wave_table.fold db ~init:[] ~f:(fun t h -> h :: t)
        |> List.map ~f:Db.Wave.to_string
        |> String.concat ~sep:"\n"
        |> send_string ~code:200
      | Some _ -> send_string ~code:200 ""

    let post_handler name_opt (content_type, raw_content_opt) =
      match name_opt, raw_content_opt with
      | None, _ ->
        send_string ~code:400 "Wave identifier is missing"
      | _, None ->
        send_string ~code:400 "Body content is missing"
      | Some name, Some body ->
        read_raw_content body >>= fun body ->
        try
          let wave = Db.Wave.of_string body in
          Db.Wave_table.set db name wave ;
          send_string ~code:200 ""
        with
        | e ->
          send_string ~code:400 (Exn.to_string e)

    let () = Eliom_registration.Any.(
      register get get_handler ;
      register post post_handler
    )
  end

end


let () =
  Bistro_server_app.register
    ~service:main_service
    (fun () () ->
      Lwt.return
        (Eliom_tools.F.html
           ~title:"bistro_server"
           ~css:[["css";"bistro_server.css"]]
           Html5.F.(body [
             h2 [pcdata "Welcome from Eliom's distillery!"];
           ])))
