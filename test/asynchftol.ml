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


(*tie-breaking for all minimize is handled by assigning the first value to be
* the minimum. is this a problem, or what we want?*)
let test_asynch_ftol_all_minimize () =
    let sq_par = Register.Int_ret_int square 
    and cs_par = Register.Int_ret_float incremental_cost in
    let a = Register.register sq_par in
    let b = Register.register cs_par in
    
    let res = Comm.init Sys.argv Comm.Fault_Tolerant Comm.Asynch  
    and my_rank = Ftol.my_rank () in
    
    Ftol.print_error ("this is my rank:"^(string_of_int (Ftol.int_of_rank my_rank)));
    let thearray = mk_array 20 5 in 
    print_array !thearray;   
    
    let minval = Parfunc.all_minimize sq_par cs_par thearray in
    
    Ftol.print_error ("the minimum value is:"^(string_of_float minval));;

let test_asynch_ftol_all_maximize () =
    
    let sq_par = Register.Int_ret_int square 
    and cs_par = Register.Int_ret_float incremental_cost in
    let a = Register.register sq_par in
    let b = Register.register cs_par in
    
    let res = Comm.init Sys.argv Comm.Fault_Tolerant Comm.Asynch  
    and my_rank = Ftol.my_rank () in
    
    Ftol.print_error ("this is my rank:"^(string_of_int (Ftol.int_of_rank my_rank)));
    let thearray = mk_array 20 5 in 
    print_array !thearray;   
    
    let minval = Parfunc.all_maximize sq_par cs_par thearray in
    
    Ftol.print_error ("the maximum value is:"^(string_of_float minval));;

let test_asynch_gather_minimum () =
    
    let sq_par = Register.Int_ret_int square 
    and cs_par = Register.Int_ret_float incremental_cost in
    let a = Register.register sq_par in
    let b = Register.register cs_par in
    
    let res = Comm.init Sys.argv Comm.Fault_Tolerant Comm.Asynch  
    and my_rank = Ftol.my_rank () in
    
    let thearray = mk_array 20 5 in 
    print_array !thearray;   
    let threshold = 0.5 in
    
    let resarray = Parfunc.gather_minimum sq_par cs_par thearray threshold in
        
    Ftol.print_error ("the resulting array is:");
    print_array !resarray;;

let test_asynch_gather_maximum () =
    
    let sq_par = Register.Int_ret_int square 
    and cs_par = Register.Int_ret_float incremental_cost in
    let a = Register.register sq_par in
    let b = Register.register cs_par in
    
    let res = Comm.init Sys.argv Comm.Fault_Tolerant Comm.Asynch  
    and my_rank = Ftol.my_rank () in
    
    let thearray = mk_array 20 5 in 
    print_array !thearray;   
    let threshold = 0.5 in
    
    let resarray = Parfunc.gather_maximum sq_par cs_par thearray threshold in
        
    Ftol.print_error ("the resulting array is:");
    print_array !resarray;;

let test_asynch_map () =
    
    let sq_par = Register.Int_ret_int square in 
    let a = Register.register sq_par in
    
    let res = Comm.init Sys.argv Comm.Fault_Tolerant Comm.Asynch  
    and my_rank = Ftol.my_rank () in
    
    let thearray = mk_array 20 5 in 
    print_array !thearray;   
    
    let resarray = Parfunc.map sq_par thearray in
        
    Ftol.print_error ("the resulting array is:");
    print_array !resarray;;
   
let test_asynch_scan_function () =
    
    let sq_par = Register.Int_ret_bool largerthanfive in 
    
    let res = Comm.init Sys.argv Comm.Fault_Tolerant Comm.Asynch  
    and my_rank = Ftol.my_rank () in
    
    let thearray = mk_array 20 5 in 
    print_array !thearray;   
    
    let resarray = Parfunc.scan sq_par thearray in
    Ftol.print_error ("the resulting array is:");
    print_array !resarray;;

test_asynch_scan_function ();;
