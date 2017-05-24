type form = {
  fields : (string * field) list ;
}
and field =
  | Int_field of int option
  | String_field of string option
  | File_field of string option
  | Form_field of form
[@@deriving sexp]

type app_specification = {
  app_title : string ;
  app_form : form ;
}
[@@deriving sexp]

type 'a run_request = {
  input : 'a ;
  files : input_file_descr list ;
}
and input_file_descr = {
  input_file_id : string ;
  input_file_md5 : string ;
}
[@@deriving sexp]

val form_value : form -> Sexplib.Type.t option
val form_files : form -> string list
