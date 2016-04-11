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
    waves : Db.Wave.t String.Map.t ;
    step_state : step_state String.Map.t ;
  }
  and step_state = Todo | Started | Done | Failed
  and step = Bistro.Workflow.step

  let empty = {
    waves = String.Map.empty ;
    step_state = String.Map.empty
  }

  let rec populate_state st =
    let open Bistro.Workflow in
    function
    | Input _ -> st
    | Select (_, u, _) -> populate_state st u
    | Step s as u ->
      let st' = List.fold s.deps ~init:st ~f:populate_state in
      if String.Map.mem st' s.id then st'
      (* if [s] is already in [st'] then the info is more uptodate
         than the contents of the bistro database *)
      else
        let state_s = if Db.in_cache db u then Done else Todo in
        String.Map.add st' ~key:s.id ~data:state_s

  let add_wave wes w =
    let open Db.Wave in
    { waves = String.Map.add wes.waves ~key:w.name ~data:w ;
      step_state =
        List.fold w.targets ~init:wes.step_state ~f:populate_state }

  let rec workflow_state wes = Bistro.Workflow.(
    function
    | Input _ -> Done
    | Select (_, u, _) -> workflow_state wes u
    | Step step ->
      String.Map.find_exn wes.step_state step.id
  )

  let rec search_available_steps wes =
    let open Bistro.Workflow in
    function
    | Input _ -> []
    | Select (_, u, _) -> search_available_steps wes u
    | Step step ->
      match String.Map.find_exn wes.step_state step.id with
      | Done | Failed | Started -> []
      | Todo ->
        let available_in_deps =
          List.fold step.deps ~init:[] ~f:(fun accu u ->
            search_available_steps wes u @ accu
          )
        in
        if available_in_deps = [] then [ step ]
        else available_in_deps

  let next wes =
    let open Bistro.Workflow in
    let available_steps =
      String.Map.fold wes.waves ~init:[] ~f:(fun ~key ~data:wave accu ->
        List.fold wave.Db.Wave.targets ~init:accu ~f:(fun accu u ->
          search_available_steps wes u @ accu
        )
      )
    in
    match available_steps with
    | [] -> wes, None
    | step :: _ ->
      { wes with
        step_state = String.Map.add wes.step_state ~key:step.id ~data:Started },
      Some step

end

(* Execution engine *)

module Exen = struct
  let wes =
    ref (
      Db.Wave_table.fold db ~init:WES.empty ~f:WES.add_wave
    )

  let add_wave w =
    Db.Wave_table.set db w.Db.Wave.name w ;
    wes := WES.add_wave !wes w ;
    return ()
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
    | Request_task () -> return None
    | I'm_alive _ -> return ()
    | Post_wave w ->
      Exen.add_wave w

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
