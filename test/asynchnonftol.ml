let print_array row =
    let size = Array.length row in
    for i=0 to size-1 do
        print_string ((string_of_int row.(i))^ " ");
    done;
    print_newline ();;

let square x = (x*x) ;;

let largerthanfive x = 
    if x > 5 then
        true
    else 
        false;;

let incremental_cost x =
    if x < 10 then
        1.0 +. ((float_of_int x)/.10.)
    else if x < 20 then
        2.0 +. ((float_of_int x)/.20.)
    else if x < 30 then
        3.0 +. ((float_of_int x)/.30.)
    else if x < 40 then
        4.0 +. ((float_of_int x)/.40.)
    else if x < 50 then
        5.0 +. ((float_of_int x)/.50.)
    else
        10.0 +.((float_of_int x)/.100.);;

let mk_array elements seed=
  let m = Array.make elements 0 in
  for i = 1 to elements do 
      Random.init i;
      m.(i - 1) <- Random.int 12;
  done;
  ref m ;;

(*testing for the asynchronous - non fault tolerant functions*)
(*---------------------------------------------------------*)
let test_asynch_map_function () =
    let res = Comm.init Sys.argv Comm.Non_Fault_Tolerant Comm.Asynch  in
    let thearray = mk_array 15 5 in
    print_array !thearray;   
    let resarray = Parfunc.map square thearray in
    print_array !resarray; 
    Comm.final ();;
   
(*scan function changes the order of the first two elements in the resulting
* array *)
let test_asynch_scan_function () =
   let res = Comm.init Sys.argv Comm.Non_Fault_Tolerant Comm.Asynch 
   and thearray = mk_array 15 5 in
   print_array !thearray;    
   let resarray = Parfunc.scan largerthanfive thearray in
   print_array !resarray;
   Comm.final ();;

(*tie-breaking for all minimize is handled by assigning the first value to be
* the minimum. is this a problem, or what we want?*)
let test_asynch_all_minimize () =
   let res = Comm.init Sys.argv Comm.Non_Fault_Tolerant Comm.Asynch 
   and thearray = mk_array 20 5 in
   print_array !thearray;    
   let minval = Parfunc.all_minimize square incremental_cost thearray in
   if ((Mpi.comm_rank Mpi.comm_world) = 0) then 
       Ftol.print_error ("the minimum value is:"^(string_of_float minval));
   Comm.final ();;

let test_asynch_all_maximize () =
   let res = Comm.init Sys.argv Comm.Non_Fault_Tolerant Comm.Asynch 
   and thearray = mk_array 20 5 in
   print_array !thearray;    
   let maxval = Parfunc.all_maximize square incremental_cost thearray in
   if ((Mpi.comm_rank Mpi.comm_world) = 0) then 
       Ftol.print_error ("the maximum value is:"^(string_of_float maxval));
   Comm.final ();;

(*the sequence of the output is changed*)
let test_asynch_gather_minimum () =
   let res = Comm.init Sys.argv Comm.Non_Fault_Tolerant Comm.Asynch 
   and thearray = mk_array 20 5 in
   print_array !thearray;    
   let threshold = 0.5 in
   let resarray = Parfunc.gather_minimum square incremental_cost thearray
   threshold in
   if ((Mpi.comm_rank Mpi.comm_world) = 0) then begin
       Ftol.print_error ("the resulting array is:");
       print_array !resarray;
   end;
   Comm.final ();;

let test_asynch_gather_maximum () =
   let res = Comm.init Sys.argv Comm.Non_Fault_Tolerant Comm.Asynch 
   and thearray = mk_array 20 5 in
   print_array !thearray;    
   let threshold = 0.5 in
   let resarray = Parfunc.gather_maximum square incremental_cost thearray
   threshold in
   if ((Mpi.comm_rank Mpi.comm_world) = 0) then begin
       Ftol.print_error ("the resulting array is:");
       print_array !resarray;
   end;
   Comm.final ();;


test_asynch_map_function ();;
