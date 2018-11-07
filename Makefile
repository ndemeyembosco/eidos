# We are not really tracking dependencies because everything is small
# enough to recompile at will.

# change to a different ocamlc if you prefer (e.g., ocamlopt)
COMPILER=ocamlc



all: clean
	$(COMPILER) -c eidosAST.ml
	ocamlyacc parse.mly
	$(COMPILER) -c parse.mli
	$(COMPILER) -c parse.ml
	ocamllex lex.mll
	$(COMPILER) -c lex.ml
	# $(COMPILER) -c prettyprint.ml
	$(COMPILER) -c main.ml
	$(COMPILER) -o main eidosAST.cmo parse.cmo lex.cmo main.cmo # order matters

clean:
	$(RM) *.cmo *.cmi parse.ml parse.mli lex.ml main *.output 