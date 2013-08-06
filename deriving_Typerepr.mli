
(** {1 Structural representations of types} *)

type 'a atomic =
  | Unit : unit atomic
  | Int : int atomic
  | Bool : bool atomic
  | String : string atomic
  | Float : float atomic
  | Int32 : int32 atomic
  | Int64 : int64 atomic

type 'a t = private
  | Atomic : 'a atomic -> 'a t
  | Tuple : 'a tuple -> 'a t
  | Function : string option * 'a t * 'b t -> ('a -> 'b) t
  | List : 'a t -> 'a list t
  | Option : 'a t -> 'a option t
  | Array : 'a t -> 'a array t
  | Ref : 'a t -> 'a ref t
  | Sum : 'a sum -> 'a t
  | Record : 'a record -> 'a t

and ('a, 'b) component = private
  | Component : 'b t * int -> ('a, 'b) component
and 'a any_component =
  | Any_component : ('a, 'b) component -> 'a any_component
and 'a tuple = { components : 'a any_component list }

and ('a, 'b) summand = private
  | Summand_nullary : int -> ('a, unit) summand
  | Summand_unary : ('a, 'b) unary_summand -> ('a, 'b) summand
  | Summand_nary : ('a, 'b) nary_summand -> ('a, 'b) summand
and ('a, 'b) unary_summand = private int * 'b t
and ('a, 'b) nary_summand = private int * 'b tuple
and 'a any_summand =
  | Any_summand : ('a, 'b) summand -> 'a any_summand
and 'a sum = private
    { summands : (string * 'a any_summand) list; }

and ('a, 'b) field = private
  | Field : int * 'b t -> ('a, 'b) field
and 'a any_field =
  | Any_field : ('a, 'b) field -> 'a any_field
and 'a record = private
    { fields : (string * 'a any_field) list; }

type any_t = Any_t : 'a t -> any_t

(** {2 Dynamically values} *)

type dyn = private
  | Dyn : 'a t * 'a -> dyn
type dyn_tuple = private
  | Dyn_tuple : 'a tuple * 'a -> dyn_tuple

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

(** {2 Inspection} *)

val get_record_field : ('a, 'b) field -> 'a -> 'b
val get_tuple_component : ('a, 'b) component -> 'a -> 'b
val get_tuple_components : 'a any_component list -> 'a -> dyn list
type 'a any_case_value =
  | Any_case_value : ('a, 'b) summand * 'b -> 'a any_case_value
val get_sum_case : 'a sum -> 'a -> string * 'a any_case_value
val get_sum_case_by_summand : ('a, 'b) summand ->'a -> 'b option

(** {2 Paths to components} *)

(** A value of type [('w, 'a, 'b, 'c) p] encodes the path from
    a value of type ['a] to one of type ['c], where ['a] is directly
    embedded in one of ['w] and ['c] in ['b]. *)
type ('w, 'a, 'b, 'c) p =
  | Root : ('w, 'a, 'w, 'a) p
  | Tuple_component : ('b, 'c) component * ('w, 'a, _, 'b) p -> ('w, 'a, 'b, 'c) p
  | List_item : int * ('w, 'a, _, 'c list) p -> ('w, 'a, 'c list, 'c) p
  | Array_item : int * ('w, 'a, _, 'c array) p -> ('w, 'a, 'c array, 'c) p
  | Case_unary : ('b, 'c) unary_summand * ('w, 'a, _, 'b) p  -> ('w, 'a, 'b, 'c) p
  | Case_nary : ('d, 'c) component * ('b, 'd) nary_summand * ('w, 'a, _, 'b) p -> ('w, 'a, 'b, 'c) p
  | Record_field : ('b, 'c) field * ('w, 'a, _, 'b) p -> ('w, 'a, 'b, 'c) p
  | Option_some : ('w, 'a, _, 'c option) p -> ('w, 'a, 'c option, 'c) p
  | Ref_content : ('w, 'a, _, 'c ref) p -> ('w, 'a, 'c ref, 'c) p

val get : 'a -> (_, 'a, _, 'c) p -> 'c option

val compose : ('w1, 'a1, 'b2, 'a2) p -> ('b2, 'a2, 'b3, 'a3) p -> ('w1, 'a1, 'b3, 'a3) p

type ('w, 'a, 'acc) folder = {
  folder : 'c 'b . 'acc -> 'c -> 'c t -> ('w, 'a, 'b, 'c) p  -> 'acc
}

val fold : ('w, 'a, 'acc) folder -> 'acc -> 'a -> 'a t -> 'acc

(** {2 Predefined type representations} *)

module type Typerepr = sig type a val t : a t end

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
val __summand_nullary__ : int -> ('a, unit) summand
val __summand_unary__ : int -> 'b t -> ('a, 'b) summand
val __summand_nary__ : int -> 'b any_component list -> ('a, 'b) summand
val __component__ : int -> 'b t -> ('a, 'b) component
val __tuple__ : 'a any_component list -> 'a t
val __sum__ : (string * 'a any_summand) list -> 'a t
