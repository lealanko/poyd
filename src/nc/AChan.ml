
open NcPrelude

type 'a t = {
    readers : 'a cont seq;
    buffer : 'a seq;
}

type 'a input = 'a t

type 'a output = 'a t

let create () = {
    readers = Seq.create ();
    buffer = Seq.create ();
}

let input c = c

let output c = c

let put c v = 
    match Seq.try_pop c.readers with
    | Some k -> wakeup k v
    | None -> ignore (Seq.push c.buffer v)
        
let take c =
    match Seq.try_pop c.buffer with
    | Some v -> return v
    | None -> ccc (Seq.push_cont c.readers)

