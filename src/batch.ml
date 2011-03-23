
module type TYPES = sig
    type a
    type b
    type c
end


module Defs (T : TYPES) = struct
    open T
    type generate_trees_arg = {
        rng : Rng.t;
        n_trees : int;
        generate : Methods.script list;
        composer : Methods.script list;
        initial_state : (a, b, c) ScriptingTypes.run;
    }
            
    type generate_trees_ret = {
        trees : (a, b) Ptree.p_tree Sexpr.t;
    }
end

module Base(T : TYPES) = struct
    module D = Defs(T)
    include T
    include D
end

module type S = sig
    module T : TYPES
    include module type of Base(T)
    val generate_trees : generate_trees_arg -> generate_trees_ret
end 


module Dummy(T : TYPES) : S with module T = T = struct
    module T = T
    include Base(T)
    let generate_trees arg = 
	assert false
end
