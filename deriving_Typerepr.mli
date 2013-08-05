
(** {1 Structural representation of types} *)

type ('a, 'b) field = private
  | Field : int * 'b t -> ('a, 'b) field
and 'a any_field =
  | Any_field : ('a, 'b) field -> 'a any_field
and 'a record = private
    { fields : (string * 'a any_field) list; }

and ('a, 'b) summand = private
  | Summand_constant : int -> ('a, unit) summand
  | Summand_alloc : int * 'b tuple -> ('a, 'b) summand
and 'a any_summand =
  | Any_summand : ('a, 'b) summand -> 'a any_summand
and 'a sum = private
    { summands : (string * 'a any_summand) list; }

and ('a, 'b) component = private
  | Component : 'b t * int -> ('a, 'b) component
and 'a any_component =
  | Any_component : ('a, 'b) component -> 'a any_component
and 'a tuple = private
  | Singleton of 'a t | Composed of 'a any_component list

and _ t = private
  | Tuple : 'a tuple -> 'a t
  | Function : string option * 'a t * 'b t -> ('a -> 'b) t
  | Unit : unit t
  | Int : int t
  | Bool : bool t
  | Int32 : int32 t
  | Int64 : int64 t
  | Float : float t
  | String : string t
  | Option : 'a t -> 'a option t
  | List : 'a t -> 'a list t
  | Array : 'a t -> 'a array t
  | Ref : 'a t -> 'a ref t
  | Sum : 'a sum -> 'a t
  | Record : 'a record -> 'a t
type any_t = Any_t : 'a t -> any_t

module type Typerepr = sig type a val t : a t end

(** {2 Dynamically values} *)

type dyn = private
  | Dyn : 'a t * 'a -> dyn
type dyn_tuple = private
  | Dyn_tuple : 'a tuple * 'a -> dyn_tuple

(** {2 Inspection} *)

val get_record_field : ('a, 'b) field -> 'a -> 'b t * 'b
val get_record_fields : 'a record -> 'a -> (string * dyn) list
val get_tuple_component : ('a, 'b) component -> 'a -> 'b
val get_tuple_components : 'a any_component list -> 'a -> dyn list
type 'a any_case_value =
  | Any_case_value : ('a, 'b) summand * 'b -> 'a any_case_value
val get_sum_case : 'a sum -> 'a -> string * 'a any_case_value
val get_sum_case_by_summand : ('a, 'b) summand ->'a -> 'b option

(** {2 Creation} *)

type 'a create_record_field = {
  create_record_field : 'b. string -> ('a, 'b) field -> 'b;
}
val create_record : 'a record -> 'a create_record_field -> 'a
type 'a create_tuple_component = {
  create_tuple_component : 'b. ('a, 'b) component -> 'b;
}
val create_tuple : 'a any_component list -> 'a create_tuple_component -> 'a
val create_sum_case : ('a, 'b) summand -> 'b -> 'a

(** {2 Predefined representations} *)

module Typerepr_unit : Typerepr with type a = unit
module Typerepr_int : Typerepr with type a = int
module Typerepr_bool : Typerepr with type a = bool
module Typerepr_int32 : Typerepr with type a = int32
module Typerepr_int64 : Typerepr with type a = int64
module Typerepr_float : Typerepr with type a = float
module Typerepr_string : Typerepr with type a = string
module Typerepr_option : functor (T : Typerepr) -> Typerepr with type a = T.a option
module Typerepr_list : functor (T : Typerepr) -> Typerepr with type a = T.a list
module Typerepr_array : functor (T : Typerepr) -> Typerepr with type a = T.a array
module Typerepr_ref : functor (T : Typerepr) -> Typerepr with type a = T.a ref

(** {2 Auxiliaries} *)

val show : 'a t -> 'a -> string

(**/**)

val __field__ : int -> 'b t -> ('a, 'b) field
val __record__ : (string * 'a any_field) list -> 'a t
val __summand_constant__ : int -> ('a, unit) summand
val __summand_alloc__ : int -> 'b tuple -> ('a, 'b) summand
val __component__ : int -> 'b t -> ('a, 'b) component
val __singleton__ : 'a t -> 'a tuple
val __composed__ : 'a any_component list -> 'a tuple
val __tuple__ : 'a tuple -> 'a t
val __sum__ : (string * 'a any_summand) list -> 'a t
