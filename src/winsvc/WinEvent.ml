open Lwt

module L = Lwt_log

type event_source

type event_type = 
    | Error
    | Warning
    | Information

external register_event_source : string -> event_source =
        "caml_winevent_register_event_source"

external deregister_event_source : event_source -> unit =
        "caml_winevent_register_event_source"

external report_event : event_source -> event_type -> string -> string -> unit =
        "caml_winevent_report_event"

let output source section level lines =
    let etype = match level with
        | L.Error | L.Fatal -> Error
        | L.Warning -> Warning
        | _ -> Information
    and sname = L.Section.name section
    in
    List.iter (fun line -> report_event source etype sname line) lines;
    return ()

let close source () = 
    deregister_event_source source;
    return ()


let logger ?(template = "$(section): $(message)") name =
    let source = register_event_source name
    in
    L.make ~output:(output source) ~close:(close source)
    
    
    
    
    



