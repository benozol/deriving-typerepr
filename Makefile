
all:
	ocamlfind c -c deriving_Typerepr.mli
	ocamlfind c -c deriving_Typerepr.ml
	ocamlfind c -c -package camlp4.quotations.o,camlp4.fulllib,deriving-ocsigen -syntax camlp4o pa_deriving_Typerepr.ml

test:
	ocamlfind c -c -package deriving-ocsigen.syntax -syntax camlp4o -ppopt pa_deriving.cma -ppopt pa_deriving_Typerepr.cmo test.ml

clean:
	rm *.cmo *.cmi

install:
	ocamlfind install deriving-typerepr META deriving_Typerepr.cmi deriving_Typerepr.cmo pa_deriving_Typerepr.cmi pa_deriving_Typerepr.cmo

uninstall:
	ocamlfind remove deriving-typerepr

check:
	camlp4o `ocamlfind query -i-format deriving-ocsigen.syntax` pa_deriving.cma pa_deriving_Typerepr.cmo -printer o test.ml
