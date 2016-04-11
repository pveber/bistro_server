[%%shared
    open Eliom_lib
    open Eliom_content
    open Html5.D

    let ( >>| ) = Lwt.( >|= )
    let ( >>= ) = Lwt.( >>= )
    let return = Lwt.return
]

open Core.Std
open Bistro_engine

let db = Db.init_exn "_bistro"

module Bistro_server_app =
  Eliom_registration.App (
    struct
      let application_name = "bistro_server"
    end)

(* Workflow Execution State *)
module WES :
sig
  type t
  val empty : t
  val add_wave : t -> Db.Wave.t -> t
  val next : t -> t * Bistro.Workflow.step option
end
=
struct
  type t = {
    waves : Db.Wave.t list ;
    step_state : step_state String.Map.t ;
  }
  and step_state = Started | Done | Failed
  and dep_state =
    | All_done
    | One_failed
    | One_running
    | One_not_started of Bistro.Workflow.u
  and step = Bistro.Workflow.step

  let empty = {
    waves = [] ;
    step_state = String.Map.empty
  }

  let rec workflow_state wes = Bistro.Workflow.(
    function
    | Input _ -> Some Done
    | Select (_, u, _) -> workflow_state wes u
    | Step step ->
      String.Map.find wes.step_state step.id
  )

  let classify_deps wes xs =
    List.fold xs ~init:All_done ~f:(fun r u ->
      let state_u = workflow_state wes u in
      match r, state_u with
      | All_done, Some Done -> All_done
      | One_not_started _, _ -> r
      | _, None -> One_not_started u
      | One_running, _
      | _, Some Started -> One_running
      | One_failed, _
      | _, Some Failed -> One_failed
    )

  let rec search_available_step wes =
    let open Bistro.Workflow in
    function
    | Input _ -> None
    | Select (_, u, _) -> search_available_step wes u
    | Step s -> (
        match String.Map.find wes.step_state s.id with
        | None -> (
            match classify_deps wes s.deps with
            | All_done -> Some s
            | One_failed | One_running -> None
            | One_not_started u ->
              search_available_step wes u
          )
        | Some _ -> None
      )

  let next wes =
    let open Bistro.Workflow in
    match
      List.find_map wes.waves ~f:(fun wave ->
        List.find_map wave.Db.Wave.targets ~f:(search_available_step wes)
      )
    with
    | None -> wes, None
    | Some u ->
      { wes with
        step_state = String.Map.add wes.step_state ~key:u.id ~data:Started },
      Some u

  let add_wave wes w =
    let open Db.Wave in
    { wes with
      waves =
        if List.exists wes.waves ~f:(fun { name } -> w.name = name)
        then List.map wes.waves ~f:(fun ({ name } as w') ->
          if name = w.name then w
          else w'
        )
        else w :: wes.waves }

end

(* Execution engine *)

module Exen = struct
  let wes = ref WES.empty
end



let main_service =
  Eliom_service.App.service ~path:[] ~get_params:Eliom_parameter.unit ()


(* Client API *)
module API = struct
  open Eliom_service.Http

  let send_string ~code body =
    Eliom_registration.String.send ~code (body, "text/plain")

  let read_raw_content ?(length = 4096) raw_content =
    let content_stream = Ocsigen_stream.get raw_content in
    Ocsigen_stream.string_of_stream length content_stream

  let handle_request
    : type a. a Bistro_client_api.request -> a Lwt.t
    =
    let open Bistro_client_api in
    function
    | Request_id () -> return ""
    | I'm_alive _ -> return ()
    | Post_wave _ -> return ()

  let request_handler = Bistro_client_api.{
    f = fun r ->
      handle_request r >>| response_serializer r
  }

  let get = service ~path:["api"] ~get_params:Eliom_parameter.unit ()
  let post = post_service ~fallback:get ~post_params:Eliom_parameter.raw_post_data ()

  let get_handler () () = send_string ~code:200 ""
  let post_handler () (content_type, raw_content_opt) =
    match raw_content_opt with
    | None ->
      send_string ~code:400 "Body content is missing"
    | Some body ->
      let open Bistro_client_api in
      read_raw_content body >>= fun body ->
      Sexp.of_string body
      |> Message.t_of_sexp
      |> request_of_message request_handler
      >>| Sexp.to_string_hum
      >>= send_string ~code:200

  let () = Eliom_registration.Any.(
    register get get_handler ;
    register post post_handler
  )
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
