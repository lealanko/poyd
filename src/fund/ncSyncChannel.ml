
open NcPrelude

module C = NcChannel

type 'a t = {
    data : 'a C.t;
    ack : unit C.t;
}

let of_channel c = {
    data = c;
    ack = C.create ();
}

let create () = of_channel (C.create ())

let put s v = 
    C.put s.data v;
    C.take s.ack

let take s =
    C.take s.data >>= fun v ->
    C.put s.ack ();
    return v
