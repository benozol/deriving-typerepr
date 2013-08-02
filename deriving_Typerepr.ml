
type name = string

type qname = name list

type param = name (* * [ `Minus | `Plus ] option *)

(* type decl = { *)
(*   params : param list; *)
(*   rhs : rhs; *)
(* }  (\* * constraint_ list * bool *\) *)

(* and rhs = *)
(*     [ `Expr of expr *)
(*     | `Fresh of expr option * repr (\* * [ `Private | `Public ] *\) *)
(*     | `Nothing *)
(*     | `Variant of variant ] *)

(* and repr = [ ] *)

type field = name * poly_expr (* * [ `Immutable | `Mutable ] *)

and summand = name * expr list

and constraint_ = expr * expr

and expr =
    [ (* `Class of [ `NYI ]
    | *) `Constr of qname * expr list
    | `Function of expr * expr
    | `Label of [ `NonOptional | `Optional ] * name * expr * expr
    (* | `Object of [ `NYI ] *)
    (* | `Param of param *)
    | `Tuple of expr list
    | `Sum of summand list
    | `Record of field list ]

and poly_expr = param list * expr

and variant = [ `Eq | `Gt | `Lt ] * tagspec list

and tagspec = [ `Tag of name * expr list | `Extends of expr ]

external (@) : ('a -> 'b) -> 'a -> 'b = "%apply"
let (@@) = List.append
let flip f x y = f y x

let list_index xs x =
  let rec aux ix = function
    | [] -> raise Not_found
    | x' :: _ when x = x' -> ix
    | _ :: xs -> aux (succ ix) xs
  in
  aux 0 xs

let ints n = Array.to_list @ Array.init n (fun ix -> ix)

let get_field (`Record (fs : field list)) (f : field) v =
  Obj.(obj @ field (repr v) (list_index fs f))

let get_component ix v =
  Obj.(obj @ field (repr v) ix)

let get_case (`Sum summands) value =
  let open Obj in
  let _, exprs as summand =
    if is_int @ repr value then
      let ix = (magic value : int) in
      flip List.nth ix @ flip List.filter summands @ fun (_, exprs) ->
        List.length exprs = 0
    else
      let () = assert (is_block @ repr value) in
      let ix = tag @ repr value in
      flip List.nth ix @ flip List.filter summands @ fun (_, exprs) ->
        List.length exprs > 0
  in
  summand,
  flip List.map (ints (List.length exprs)) @ fun ix ->
    obj @ field (repr value) ix

module type Typerepr =
sig
  type a
  val t : expr
end
type 'a t = expr
module Typerepr_unit = struct type a = unit let t : expr = `Constr (["unit"], []) end
module Typerepr_int = struct type a = int let t : expr = `Constr (["int"], []) end
module Typerepr_bool = struct type a = bool let t : expr = `Constr (["bool"], []) end
module Typerepr_int32 = struct type a = int32 let t : expr = `Constr (["int32"], []) end
module Typerepr_int64 = struct type a = int64 let t : expr = `Constr (["int64"], []) end
module Typerepr_float = struct type a = float let t : expr = `Constr (["float"], []) end
module Typerepr_string = struct type a = string let t : expr = `Constr (["string"], []) end
module Typerepr_option (T : Typerepr) : Typerepr with type a = T.a option = struct
  type a = T.a option
  let t : expr = `Constr (["option"], [T.t])
end
module Typerepr_list (T : Typerepr) : Typerepr with type a = T.a list = struct
  type a = T.a list
  let t : expr = `Constr (["list"], [T.t])
end
module Typerepr_array (T : Typerepr) : Typerepr with type a = T.a array = struct
  type a = T.a array
  let t : expr = `Constr (["array"], [T.t])
end
module Typerepr_ref (T : Typerepr) : Typerepr with type a = T.a ref = struct
  type a = T.a ref
  let t : expr = `Constr (["ref"], [T.t])
end
