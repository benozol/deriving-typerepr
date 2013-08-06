
open Pa_deriving_common

external (@) : ('a -> 'b) -> 'a -> 'b = "%apply"
let (@@) = List.append
let flip f x y = f y x
let flip2 f x y z = f z x y
let option_map f = function
  | None -> None
  | Some x -> Some (f x)

module Description : Defs.ClassDescription = struct
  let classname = "Typerepr"
  let runtimename = "Deriving_Typerepr"
  let default_module = None
  let alpha = None
  let allow_private = true
  let predefs = [
    ["int" ], ["Deriving_Typerepr";"int"];
    ["bool" ], ["Deriving_Typerepr";"bool"];
    ["unit" ], ["Deriving_Typerepr";"unit"];
    ["char" ], ["Deriving_Typerepr";"char"];
    ["int32" ], ["Deriving_Typerepr";"int32"];
    ["int64" ], ["Deriving_Typerepr";"int64"];
    ["float" ], ["Deriving_Typerepr";"float"];
    ["string" ], ["Deriving_Typerepr";"string"];
    ["list" ], ["Deriving_Typerepr";"list"];
    ["ref" ], ["Deriving_Typerepr";"ref"];
    ["option" ], ["Deriving_Typerepr";"option"];
    ["array" ], ["Deriving_Typerepr";"array"];
  ]
  let depends = []
end

module Builder (Loc : Defs.Loc) = struct

  module Helpers = Base.AstHelpers(Loc)
  module Generator = Base.Generator(Loc)(Description)

  open Loc
  open Camlp4.PreCast

  let wrap_params params =
    Helpers.expr_list (List.map (fun str -> <:expr< $`str:str$ >>) params)

  let generator = (object (self)

    inherit Generator.generator

    method proxy () =
      None, [ <:ident< t >> ;
              <:ident< dyn >> ]

    method tuple ctxt args =
      let exp =
        let components =
          flip List.mapi args @ fun ix arg ->
            let exp = self#call_expr ctxt arg "t" in
            <:expr< Any_component (__component__ $`int:ix$ $exp$) >>
        in
        <:expr<
          __tuple__ $Helpers.expr_list components$
        >>
      in
      [ <:str_item<
          let t = let open Deriving_Typerepr in $exp$
        >> ]

    method record ?eq ctxt name args constraints fields =
      let fields =
        flip List.mapi fields @ fun ix (name, (vars, exp), mutability) ->
          let exp = self#call_expr ctxt exp "t" in
          <:expr< $`str:name$, Any_field (__field__ $`int:ix$ $exp$) >>
      in
      let exp = <:expr< __record__ $Helpers.expr_list fields$ >> in
      [ <:str_item<
          let rec t = let open Deriving_Typerepr in $exp$
        >> ]

    method sum ?eq ctxt name args constraints summands =
      let summands =
        List.rev @ snd @
          flip2 List.fold_left ((0,0), []) summands @
            fun ((constant_ix, alloc_ix), sofar) (name, exps) ->
              let constant_ix, alloc_ix, summand =
                if exps = [] then
                  succ constant_ix, alloc_ix,
                  <:expr< __summand_nullary__ $`int:constant_ix$ >>
                else
                  constant_ix, succ alloc_ix,
                  match exps with
                    | [ exp ] ->
                      let exp = self#call_expr ctxt exp "t" in
                      <:expr< __summand_unary__  $`int:alloc_ix$ $exp$ >>
                      | _ ->
                        let components =
                          flip List.mapi exps @ fun ix exp ->
                            let exp = self#call_expr ctxt exp "t" in
                            <:expr< Any_component (__component__ $`int:ix$ $exp$) >>
                        in
                        <:expr< __summand_nary__  $`int:alloc_ix$ $Helpers.expr_list components$ >>
              in
              (constant_ix, alloc_ix),
              <:expr< $`str:name$, Any_summand $summand$ >> :: sofar
      in
      let exp = <:expr< __sum__ $Helpers.expr_list summands$ >> in
      [ <:str_item<
          let rec t = let open Deriving_Typerepr in $exp$
        >> ]

    method variant = Base.fatal_error _loc "Variants not yet supported"
    (* method class_ = Base.fatal_error _loc "Classes not yet supported" *)
    (* method object_ = Base.fatal_error _loc "Objects not yet supported" *)
    (* method label = Base.fatal_error _loc "Objects not yet supported" *)

    (* method function_ ctxt (exp, exp') = *)
    (*   let exp = self#call_expr ctxt exp "t" in *)
    (*   let exp' = self#call_expr ctxt exp' "t" in *)
    (*    [ <:str_item< let t = `Function ($exp$, $exp'$) >> ] *)

  end :> Generator.generator)

  let classname = Description.classname
  let runtimename = Description.runtimename
  let generate = Generator.generate generator
  let generate_sigs = Generator.generate_sigs generator
  let generate_expr = Generator.generate_expr generator

end


module Typerepr = Base.Register(Description)(Builder)

let depends = (module Builder : Defs.FullClassBuilder)
