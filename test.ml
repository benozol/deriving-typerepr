
type t0 = unit deriving (Typerepr)
type t1 = int deriving (Typerepr)
type t2 = int * int deriving (Typerepr)
type t3 = A | B of int deriving (Typerepr)
type t4 = { x : int ; y : int * int } deriving (Typerepr)
type t5 = C of t4 option deriving (Typerepr)
