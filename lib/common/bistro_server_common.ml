open Sexplib.Std

type form_value =
  | Int of int
  | String of string
  | Record of (string * form_value) list
[@@deriving sexp]

type form = {
  fields : (string * field) list ;
}
and field =
  | Int_field of int option
  | String_field of string option
  | Form_field of form
[@@deriving sexp]

type app_specification = {
  app_title : string ;
  app_form : form ;
}
[@@deriving sexp]

