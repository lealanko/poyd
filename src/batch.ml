module Defs (T : ScriptingTypes.TYPES) = struct
    module SD = ScriptingTypes.Defs(T)
    open SD
    type generate_trees_arg = {
        rng : Rng.t;
        n_trees : int;
        generate : Methods.script list;
        composer : Methods.script list;
        initial_state : r;
    }
            
    type generate_trees_ret = {
        trees : tree Sexpr.t;
    }
end


module type S = sig
    module T : ScriptingTypes.TYPES
    include module type of Defs(T)
    val generate_trees : generate_trees_arg -> generate_trees_ret
end 


module Dummy(T : ScriptingTypes.TYPES) : S with module T = T = struct
    module T = T
    include Defs(T)
    let generate_trees arg = 
	assert false
end
