Code snippet regarding concurrency (Mutex, Condition, Thread) in OCaml.

- `pro_con.ml`: Producers-Consumers Problem
- `concurrency.ml` and `bank.ml` are from [CS3110 of Cornell](http://www.cs.cornell.edu/courses/cs3110/2013sp/recitations/rec17.html)

# Usage

	ocamlmktop -thread unix.cma threads.cma -o mytop
	./mytop -I +threads
	# test ();;
	
