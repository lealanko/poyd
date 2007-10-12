INCLUDE1 = ../ocamlmpi/ 
INCLUDE2 = ../src

DEPEND = unix.cma ../src/parfunc.o ../src/ftol.cmo ../src/register.cmo ../src/process.cmo ../src/asynch.cmo ../src/array_ops.cmo ../src/comm.cmo ../src/paroperas.cmo ../src/parparam.cmo ../src/paroper.cmo ../src/parfunc.cmo

clean:	
	rm -f *.cmi *.cmo *.cmx

testasynchftol: 
	ocamlc -cc mpicc -I $(INCLUDE1) -I $(INCLUDE2) ../ocamlmpi/mpi.cma $(DEPEND) asynchftol.ml

testasynchnonftol: 
	ocamlc -cc mpicc -I $(INCLUDE1) -I $(INCLUDE2) ../ocamlmpi/mpi.cma $(DEPEND) asynchnonftol.ml

testsynch: 
	ocamlc -cc mpicc -I $(INCLUDE1) -I $(INCLUDE2) ../ocamlmpi/mpi.cma $(DEPEND) synch.ml
