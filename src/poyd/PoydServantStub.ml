open PoydPrelude
open PoydDefs

module Client = PoydClientStub

type 'a consumer = ('a, unit) handle
type 'a producer = (unit, 'a option) handle

type 'r tu = (unit, 'r) teq

type ('a, 'r) th = ((unit, 'a) handle, 'r) teq

type 'a field =
    | Data of (Data.d, 'a) teq
    | Rng of (PoyRandom.t, 'a) teq
    | Support of support_type * (support_class, 'a) teq
    | Bremer of (Methods.support_tree Sexpr.t, 'a) teq
    | Run of (r, 'a) teq
    | Trees of (tree Sexpr.t, 'a) teq
    | StoredTrees of (tree Sexpr.t, 'a) teq

module type SET = sig
    type a
    val field : a field
    val value : a
end

type 'r msg = 
    | SetClient of Client.t * 'r tu
    | BeginScript of script list * 'r tu
    | FinalReport of 'r tu
    | Get of 'r field
    | Set of (module SET) * 'r tu
    | GetOutput of (output list, 'r) teq
    | ClearOutput of 'r tu
    | AddTrees of bool * tree Sexpr.t * 'r tu
    | SaveOrigTrees of 'r tu
    | ClearOrigTrees of 'r tu
    | BeginOnEachTree of script list * script list * 
            ((PoyRandom.t * tree, unit) handle * (unit, unit) handle, 'r) teq
    | BeginSupport of Methods.support_method * 
            ((PoyRandom.t, unit) handle * (unit, unit) handle, 'r) teq

type t = {
    name : string;
    hdl : 'r . ('r msg, 'r) handle;
}

let get_name s =
    return s.name

let set_client s c =
    s.hdl $ SetClient(c, eq_refl)

let begin_script s script =
    s.hdl $ BeginScript(script, eq_refl)

let final_report s =
    s.hdl $ FinalReport(eq_refl)

let set (type a_) (field : a_ field) s (value : a_) =
    let module Set = struct
        type a = a_
        let field = field
        let value = value
    end 
    in
    s.hdl $ Set ((module Set : SET), eq_refl)

let get field s =
    s.hdl $ Get(field)

let getset field = (get field, set field)


let get_trees, set_trees = 
    getset (Trees(eq_refl))

let get_stored_trees, set_stored_trees =
    getset (Trees(eq_refl))

let add_trees s trees =
    s.hdl $ AddTrees(false, trees, eq_refl)

let add_stored_trees s trees =
    s.hdl $ AddTrees(true, trees, eq_refl)

let get_data, set_data =
    getset (Data(eq_refl))

(* XXX: send in parts! This can be huge. *)
let get_run, set_run =
    getset (Run(eq_refl))
    
let get_rng, set_rng =
    getset (Rng(eq_refl))

let clear_output s =
    s.hdl $ ClearOutput(eq_refl)

let get_output s =
    s.hdl $ GetOutput(eq_refl)

let save_original_trees s =
    s.hdl $ SaveOrigTrees(eq_refl)

let clear_original_trees s =
    s.hdl $ ClearOrigTrees(eq_refl)

let begin_oneachtree s dosomething mergingscript = 
    s.hdl $ BeginOnEachTree (dosomething, mergingscript, eq_refl) 
    >>= fun (iter_h, finish_h) ->
    return ((fun tree rng -> iter_h $ (tree, rng)),
            (fun () -> finish_h $ ()))

let begin_support s meth =
    s.hdl $ BeginSupport (meth, eq_refl) >>= fun (iter_h, finish_h) ->
    return ((fun rng -> iter_h $ rng), (fun () -> finish_h $ ()))

let get_support s typ = 
    get (Support (typ, eq_refl)) s

let set_support s typ =
    set (Support (typ, eq_refl)) s

let get_bremer =
    get (Bremer(eq_refl))


module Make (Servant : SERVANT with module Client = Client) = struct
    open Servant

    let (>:) t teq = 
        Lwt.map (cast teq) t


        
    let f s = function
        | SetClient(c, teq) ->
            set_client s c >: teq
        | BeginScript(script, teq) ->
            begin_script s script >: teq
        | FinalReport(teq) ->
            final_report s >: teq
        | Set (setm, tu) -> begin
            let module S = (val setm : SET)
            in
            let (<:<) setter teq = setter s (cast_back teq S.value) >: tu
            in
            match S.field with
            | Data(teq) -> set_data <:< teq
            | Rng(teq) -> set_rng <:< teq
            | Support(typ, teq) -> return () >: tu
            | Run(teq) -> set_run <:< teq
            | Trees(teq) -> set_trees <:< teq
            | StoredTrees(teq) -> set_stored_trees <:< teq
            | Bremer(teq) -> return () >: tu
        end
        | Get(field) -> begin
            match field with
            | Data(teq) -> get_data s >: teq
            | Rng(teq) -> get_rng s >: teq
            | Support(typ, teq) -> get_support s typ >: teq
            | Run(teq) -> get_run s >: teq
            | Trees(teq) -> get_trees s >: teq
            | StoredTrees(teq) -> get_stored_trees s >: teq
            | Bremer(teq) -> return `Empty >: teq
        end
        | AddTrees(false, trees, teq) -> 
            add_trees s trees >: teq
        | AddTrees(true, trees, teq) ->
            add_stored_trees s trees >: teq
        | SaveOrigTrees(teq) ->
            save_original_trees s >: teq
        | ClearOrigTrees(teq) ->
            clear_original_trees s >: teq
        | GetOutput(teq) ->
            get_output s >: teq
        | ClearOutput(teq) ->
            clear_output s >: teq
        | BeginOnEachTree (todo, combine, teq) ->
            begin_oneachtree s todo combine >>= fun (iter, finish) ->
            let iter_h = publish2 iter 
            in
            let finish_r = ref None
            in
            let finish_h = publish (fun () ->
                withdraw iter_h;
                withdraw (BatOption.get !finish_r);
                finish ())
            in
            finish_r := Some finish_h;
            return (iter_h, finish_h) >: teq
         | BeginSupport (meth, teq) ->
             begin_support s meth >>= fun (iter, finish) ->
             let iter_h = publish iter
             in
             let finish_r = ref None
             in
             let finish_h = publish (fun () ->
                 withdraw iter_h;
                 withdraw (BatOption.get !finish_r);
                 finish ())
             in
             finish_r := Some finish_h;
             return (iter_h, finish_h) >: teq
    let create s = 
        get_name s >>= fun name ->
        return {
            name = name;
            hdl = fun (type r) -> publish (f s);
        }
end
