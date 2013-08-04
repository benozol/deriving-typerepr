Deriving runtime type representation
====================================

This is kind of a meta-deriving class as it provides a runtime representation of OCaml types; well-typed thanks to GADTs

```ocaml
type my = A of int option * string | B deriving (Typerepr)
```

Now, a value `(v : my)` can be inspected or generated with type representation `Typerepr.t<my>` and the auxiliaries in `Deriving_Typerepr`.  

Take it as a small consolation as we are waiting for the [native implementation](https://gitorious.org/ocaml-ty)  of runtime type representations in OCaml ☺.
