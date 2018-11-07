# We are not really tracking dependencies because everything is small
# enough to recompile at will.

# change to a different ocamlc if you prefer (e.g., ocamlopt)
COMPILER=ocamlc

all: clean
	# $(COMPILER) -c ast.ml
	ocamlyacc parse.mly
	$(COMPILER) -c parse.mli
	$(COMPILER) -c parse.ml
	ocamllex lex.mll
	$(COMPILER) -c lex.ml
	# $(COMPILER) -c interp.ml
	$(COMPILER) -c main.ml
	$(COMPILER) -o interp parse.cmo lex.cmo main.cmo # order matters

clean:
	$(RM) *.cmo *.cmi parse.ml parse.mli lex.ml interp
