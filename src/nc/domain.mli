type key

module type S = sig
    val key : key
    val request : request -> response lwt
    val 
end
