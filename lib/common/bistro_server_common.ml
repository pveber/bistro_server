open Sexplib.Std
module Option = CCOpt

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

let rec form_value { fields } =
  let open Option in
  let field (lab, value) =
    field_value value >|= fun f ->
    sexp_of_list CCFun.id [ sexp_of_string lab ; f ]
  in
  List.map field fields
  |> sequence_l
  >|= sexp_of_list CCFun.id

and field_value =
  let open Option in
  function
  | Int_field (Some i) -> return @@ sexp_of_int i
  | String_field (Some s) -> return @@ sexp_of_string s
  | Form_field f -> form_value f
  | Int_field None | String_field None -> None
