open PoydPrelude

type worker = PoydServantStub.t

type consumer = {
    priority : int;
    cont : worker cont;
}

module C = struct
    type t = consumer
    let compare c1 c2 = BatInt.compare c1.priority c2.priority
end

module Q = BatHeap.Make(C)

type t = {
    mutable available : worker BatSet.t;
    mutable pending_consumers : Q.t;
}

let create () = {
    available = BatSet.empty;
    pending_consumers = Q.empty;
}

let get p pri =
    try
        let w, rest = BatSet.pop p.available in
        p.available <- rest;
        return w
    with Not_found ->
        ccc (fun k ->
            let con = { priority = pri; cont = k } in
            p.pending_consumers <- Q.insert p.pending_consumers con)

let put p w = return begin
    if Q.size p.pending_consumers = 0 then
        p.available <- BatSet.add w p.available
    else
        let con = Q.find_min p.pending_consumers in
        p.pending_consumers <- Q.del_min p.pending_consumers;
        Lwt.wakeup con.cont w end
        
        

