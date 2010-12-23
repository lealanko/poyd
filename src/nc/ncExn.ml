

type descriptor = {
    name : string
}

let table : (string * int, descriptor) Hashtbl.t =
    Hashtbl.create 7;;


let descriptor (exn : exn) : descriptor =
    Obj.obj (Obj.field (Obj.repr exn) 0)

let set_descriptor exn dtor : unit =
    Obj.set_field (Obj.repr exn) 0 (Obj.repr dtor)

let size (exn : exn) : int =
    Obj.size (Obj.repr exn)


let register exn = 
    let d = descriptor exn in
    Hashtbl.add table (d.name, size exn) d

let immigrate exn =
    let d = descriptor exn in
    let key = (d.name, size exn) in
    if Hashtbl.mem table key then
        let d2 = Hashtbl.find table (d.name, size exn) in
        set_descriptor exn d2
        
    
    

let _ =
    List.iter register
        [Out_of_memory;
         Sys_error "";
         Failure "";
         Invalid_argument "";
         End_of_file;
         Division_by_zero;
         Not_found;
         Match_failure ("", 0, 0);
         Stack_overflow;
         Sys_blocked_io;
         Assert_failure ("", 0, 0);
         Undefined_recursive_module ("", 0, 0)]
    
