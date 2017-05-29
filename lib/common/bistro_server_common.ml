open Sexplib.Std
module Option = CCOpt
module List = CCListLabels

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

let rec form_value { fields } =
  let open Option in
  let field (lab, value) =
    field_value value >|= fun f ->
    sexp_of_list CCFun.id [ sexp_of_string lab ; f ]
  in
  List.map ~f:field fields
  |> sequence_l
  >|= sexp_of_list CCFun.id

and field_value =
  let open Option in
  function
  | Int_field (Some i) -> return @@ sexp_of_int i
  | String_field (Some s)
  | File_field (Some s) -> return @@ sexp_of_string s
  | Form_field f -> form_value f
  | Int_field None
  | File_field None
  | String_field None -> None


let form_files form =
  let rec traverse_form { fields } =
    List.flat_map fields ~f:(fun (_, field) ->
        traverse_field field
      )

  and traverse_field = function
    | File_field (Some fn) -> [ fn ]
    | Form_field form -> traverse_form form
    | Int_field _ | String_field _
    | File_field _ -> []

  in
  traverse_form form
