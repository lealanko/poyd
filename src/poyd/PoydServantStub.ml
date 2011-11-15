open PoydPrelude
open PoydDefs

module Client = PoydClientStub

type 'a consumer = ('a, unit) handle
type 'a producer = (unit, 'a option) handle


type msg = 
    | SetClient of Client.t
    | BeginScript of script list
    | FinalReport
    | SetTrees of bool * bool * tree producer
    | GetTrees of bool * tree consumer
    | SetData of Data.d
    | GetData of Data.d consumer
    | SetRng of PoyRandom.t
    | GetRng of PoyRandom.t consumer
    | GetOutput of output list consumer
    | ClearOutput
    | SaveOrigTrees
    | ClearOrigTrees
    | BeginOnEachTree of script list * script list * 
            (((PoyRandom.t * tree, unit) handle) *
                    (unit, unit) handle) consumer
    | BeginSupport of Methods.support_method * 
            ((PoyRandom.t, unit) handle * (unit, unit) handle) consumer
    | GetSupport of support_type * support_class consumer
    (* XXX: these are temporary, should be split *)
    | SetRun of r
    | GetRun of r consumer
            
type t = {
    name : string;
    hdl : (msg, unit) handle;
}

let get_name s =
    return s.name

let set_client s c =
    s.hdl $ SetClient(c)

let begin_script s script =
    s.hdl $ BeginScript(script)

let final_report s =
    s.hdl $ FinalReport

let tree_producer trees =
    (* Ideally we would iterate the sexpr directly with an explicit stack, 
       but this is okay. *)
    let arr = Array.of_list (Sexpr.to_list trees) in
    let ir = ref 0 in
    let get () =
        let i = !ir in
        if i = Array.length arr
        then None
        else begin
            incr ir;
            Some (Array.get arr i)
        end in
    (fun () -> return (get ()))
    
let set_trees_gen storedp addp s trees =
    with_handle (tree_producer trees)
        (fun producer -> s.hdl $ SetTrees(storedp, addp, producer))

let set_trees =
    set_trees_gen false false

let set_stored_trees =
    set_trees_gen true false

let add_trees =
    set_trees_gen false true

let add_stored_trees =
    set_trees_gen true true

let tree_consumer () =
    let lst = ref [] in
    let send tree =
        lst := tree :: !lst in
    ((fun tree -> return (send tree)),
     (fun () -> return (Sexpr.of_list (List.rev !lst))))
    
let get_trees_gen storedp s =
    let consume, get = tree_consumer () in
    with_handle consume
        (fun consumer -> s.hdl $ GetTrees(storedp, consumer)) >>= fun () ->
    get ()

let get_trees =
    get_trees_gen false

let get_stored_trees =
    get_trees_gen true

let set_data s data =
    s.hdl $ SetData(data)

let get_with_consumer fn =
    let r = ref None in
    with_handle (fun v -> r := Some v; return ()) fn >>= fun () ->
    return (BatOption.get !r)
        
let get_data s =
    get_with_consumer (fun consumer -> s.hdl $ GetData consumer)

(* XXX: send in parts! This can be huge. *)
let set_run s r =
    s.hdl $ SetRun(r)

let get_run s =
    get_with_consumer (fun consumer -> s.hdl $ GetRun consumer)
    
let set_rng s rng =
    s.hdl $ SetRng(rng)

let get_rng s = 
    get_with_consumer (fun consumer -> s.hdl $ GetRng consumer)

let clear_output s =
    s.hdl $ ClearOutput

let get_output s =
    get_with_consumer (fun consumer -> s.hdl $ GetOutput consumer)

let save_original_trees s =
    s.hdl $ SaveOrigTrees

let clear_original_trees s =
    s.hdl $ ClearOrigTrees

let acquire_trees producer = 
    let rec aux acc =
        producer $ () >>= function
          | None -> return (Sexpr.of_list (List.rev acc))
          | Some tree -> aux (tree :: acc) in
    aux []

let provide_trees consumer trees =
    Lwt_list.iter_s 
        (fun tree -> consumer $ tree)
        (Sexpr.to_list trees)

let begin_oneachtree s dosomething mergingscript = 
    get_with_consumer (fun consumer ->
        s.hdl $ BeginOnEachTree (dosomething, mergingscript, consumer))
    >>= fun (iter_h, finish_h) ->
    return ((fun tree rng -> iter_h $ (tree, rng)),
            (fun () -> finish_h $ ()))

let begin_support s meth =
    get_with_consumer (fun consumer ->
        s.hdl $ BeginSupport (meth, consumer)) >>= fun (iter_h, finish_h) ->
    return ((fun rng -> iter_h $ rng), (fun () -> finish_h $ ()))

let get_support s typ =
    get_with_consumer (fun consumer ->
        s.hdl $ GetSupport (typ, consumer))

module Make (Servant : SERVANT with module Client = Client) = struct
    open Servant
    let f (s : t) : msg -> unit lwt = function
        | SetClient c ->
            set_client s c
        | BeginScript(script) ->
            begin_script s script
        | FinalReport ->
            final_report s
        | SetTrees(storedp, addp, producer) -> begin
            acquire_trees producer >>= fun trees ->
            match storedp, addp with
            | false, false -> set_trees s trees
            | false, true -> add_trees s trees
            | true, false -> set_stored_trees s trees
            | true, true -> add_stored_trees s trees
        end
        | GetTrees(storedp, consumer) -> 
            (if storedp then get_stored_trees else get_trees) s >>= 
                fun trees ->
            provide_trees consumer trees
        | SetData(data) ->
            set_data s data
        | SetRun(r) ->
            set_run s r
        | GetRun(consumer) ->
            get_run s >>= fun r ->
            consumer $ r
        | GetData(consumer) -> 
            get_data s >>= fun data ->
            consumer $ data
        | SetRng(rng) -> 
            set_rng s rng
        | SaveOrigTrees ->
            save_original_trees s
        | ClearOrigTrees ->
            clear_original_trees s
        | GetOutput(consumer) ->
            get_output s >>= fun o ->
            consumer $ o
        | ClearOutput ->
            clear_output s
        | GetRng(consumer) -> 
            get_rng s >>= fun data ->
            consumer $ data
        | BeginOnEachTree (todo, combine, consumer) ->
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
            consumer $ (iter_h, finish_h)
         | BeginSupport (meth, consumer) ->
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
             consumer $ (iter_h, finish_h)
         | GetSupport (typ, consumer) ->
             get_support s typ >>= fun sup ->
             consumer $ sup
    let create s = 
        get_name s >>= fun name ->
        return {
            name = name;
            hdl = publish (f s);
        }
end
