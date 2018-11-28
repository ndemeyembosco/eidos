# We are not really tracking dependencies because everything is small
# enough to recompile at will.

# change to a different ocamlc if you prefer (e.g., ocamlopt)
COMPILER=ocamlc



all: clean
	$(COMPILER) -c eidosAST.ml
	ocamlyacc parse.mly
	$(COMPILER) -c parse.mli
	$(COMPILER) -c parse.ml
	ocamlyacc parse2.mly
	$(COMPILER) -c parse2.mli
	$(COMPILER) -c parse2.ml
	ocamllex lex.mll
	ocamllex lex2.mll
	$(COMPILER) -c lex.ml
	$(COMPILER) -c lex2.ml
	$(COMPILER) -c eidosTypes.ml
	$(COMPILER) -c eidosInterp.ml
	$(COMPILER) -o interp eidosAST.cmo eidosTypes.cmo parse.cmo parse2.cmo lex.cmo lex2.cmo eidosInterp.ml # order matters

clean:
	$(RM) *.cmo *.cmi parse.ml parse.mli lex.ml parse *.output parse 
