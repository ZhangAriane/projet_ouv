projet.exe: projet.ml
	ocamlfind ocamlc -linkpkg -package unix -o projet.exe projet.ml

run: projet.exe 
	.\projet.exe
	dot -Tpng arbre_decision.dot -o arbre_decision.png
	dot -Tpng zdd.dot -o zdd.png
	dot -Tpng zdd_bis.dot -o zdd_bis.png

clean: 
	del projet.exe 
	del projet.cmi 
	del projet.cmo 
	del *.dot  

cleanAll:
	del projet.exe 
	del projet.cmi 
	del projet.cmo 
	del *.dot 
	del *.png
	del *.csv

	