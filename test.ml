
type t0 = unit deriving (Typerepr)
type t1 = int deriving (Typerepr)
type t2 = int * int deriving (Typerepr)
type t3 = A | B of int | C of int * int deriving (Typerepr)
type t4 = { x : int ; y : int * int } deriving (Typerepr)
type t5 = C of t4 option deriving (Typerepr)
type t6 = [`A|`B of int|`C|`D of int * float] deriving (Typerepr)
