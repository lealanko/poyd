
module type T = sig
    type a
    type b
    type c
end

module type S = sig
    include T
    val generate_trees :
	n_trees : int ->
	generate : Methods.script list ->
	composer : Methods.script list ->
	initial_state : (a, b, c) ScriptingTypes.run ->
	(a, b) Ptree.p_tree Sexpr.t
end


module Dummy (T : T) :
    S with type a = T.a with type b = T.b with type c = T.c = 
struct
    include T
    let generate_trees ~n_trees ~generate ~composer ~initial_state =
	assert false
end
