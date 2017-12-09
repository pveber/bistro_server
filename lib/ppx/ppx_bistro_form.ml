open Ppx_core
open Ast_builder.Default

let ( @@ ) = Caml.( @@ )

module Type_conv = Ppx_type_conv.Std.Type_conv

module Attrs = struct
  let file =
    Attribute.declare "file"
      Attribute.Context.label_declaration
      Ast_pattern.(pstr nil)
      `file
end

module Str = struct
  let rec form_expr_of_td loc td =
    match td.ptype_kind with
    | Ptype_record labels ->
      let f ({ pld_name = { txt = label ; _ } ;
              pld_type = field_ty ; _ } as ld) =
        let attrs = [
          Attribute.get Attrs.file ld
        ] |> List.filter_opt
        in
        [%expr [%e estring ~loc label], [%e field_expr_of_ty ~attrs loc field_ty ]]
      in
      let fields = List.map labels ~f in
      [%expr { Bistro_server_common.fields = [%e elist ~loc fields] } ]
    | Ptype_abstract
    | Ptype_variant _
    | Ptype_open -> failwith "only records can be derived to bistro forms"

  and field_expr_of_ty loc ~attrs { ptyp_desc ; _ } =
    match ptyp_desc with
    | Ptyp_constr ({ txt = ident ; _ }, _) -> (
        match ident with
        | Lident "int" -> [%expr Bistro_server_common.Int_field None]
        | Lident "string" ->
          if List.mem ~equal:Caml.( = ) attrs `file then
            [%expr Bistro_server_common.File_field None]
          else
            [%expr Bistro_server_common.String_field None]
        | Lident x ->
          let fun_name = x ^ "_bistro_form" in
          [%expr Bistro_server_common.Form_field [%e evar ~loc fun_name]]
        | _ -> failwith "Unsupported field type for bistro forms"
      )

    | _ -> failwith "Unsupported field type for bistro forms"

  let form_of_td td =
    let td = name_type_params_in_td td in
    let ty_name = td.ptype_name.txt in
    let loc = td.ptype_loc in
    let pat = pvar ~loc (ty_name ^ "_bistro_form") in
    let body = form_expr_of_td loc td in
    value_binding ~loc ~pat ~expr:body

  let form_of_tds ~loc ~path:_ (rec_flag, tds) =
    let bindings = List.map tds ~f:form_of_td in
    pstr_value_list ~loc rec_flag bindings

end

module Sig = struct

  let form_of_tds ~loc:_ ~path:_ (_, tds) =
    List.map tds ~f:(fun td ->
        let loc = td.ptype_loc in
        psig_value ~loc @@ value_description ~loc
          ~name:(Located.map (Fn.flip (^) "_bistro_form") td.ptype_name)
          ~type_:[%type: Bistro_server_common.form]
          ~prim:[]
      )
end

let str_type_decl =
  Type_conv.Generator.make_noarg Str.form_of_tds ~attributes:[
    Attribute.T Attrs.file
  ]

let sig_type_decl =
  Type_conv.Generator.make_noarg Sig.form_of_tds ~attributes:[]

let _bistro_form = Type_conv.add "bistro_form" ~str_type_decl ~sig_type_decl
