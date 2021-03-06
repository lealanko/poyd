open FundPrelude

exception RouteError
exception ConnectionError of exn

type ('a, 'r) handle

val ($) : ('a, 'r) handle -> 'a -> 'r lwt

val publish : ('a -> 'r lwt) -> ('a, 'r) handle
val publish2: ('a -> 'b -> 'r lwt) -> (('a * 'b), 'r) handle
val publish3 : ('a -> 'b -> 'c -> 'r lwt) -> (('a * 'b * 'c), 'r) handle

val withdraw : ('a, 'r) handle -> unit

val with_handle : ('a -> 'r lwt) -> (('a, 'r) handle -> 'b lwt) -> 'b lwt

type connection

val connect : 
    ?addr:Unix.sockaddr -> 
    ?host:string ->
    ?port:int ->
    ?path:string ->
    unit ->
    connection lwt

val disconnect : connection -> unit lwt

val wait : connection -> unit lwt

val get_root : string -> 'a lwt

val set_root : string -> 'a -> unit lwt

val listen :
    ?addr:Unix.sockaddr -> 
    ?local:bool ->
    ?port:int ->
    ?path:string ->
    unit ->
    unit lwt

(* val finish : unit -> unit lwt *)


