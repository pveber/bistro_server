open Bistro_utils
open Bistro_server_common

module type App = sig
  type input
  [@@deriving sexp, bistro_form]

  val title : string
  val input_bistro_form : form
  val derive :
    data:(string -> string) ->
    input ->
    Repo.t
end

module Make(App : App) : sig
  val start :
    ?port:int ->
    ?build_log:bool ->
    ?root_dir:string ->
    unit -> unit Lwt.t
end
