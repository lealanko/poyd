type msg_type =
    | From_Master
    | From_Slave

let min a b =
    if a > b then b else a;;

let mkmatrix rows cols seed=
  Random.init seed;
  let m = Array.make_matrix rows cols 0 in
  for i = 0 to (rows - 1) do
      for j = 0 to (cols - 1) do 
          m.(i).(j) <- Random.int 10;
          done;
  done;
  (m,rows,cols) ;;

let rec inner_mult last_row accumulator m1i m2 col_no =
  if last_row < 0 then accumulator
  else 
      inner_mult (last_row - 1) (accumulator + m1i.(last_row) *
      m2.(last_row).(col_no)) m1i m2 col_no;;

let mmult rows cols m1 m2 mr =
  let last_col = cols - 1 and last_row = rows - 1 in
  for i = 0 to last_row do
      let m1i = m1.(i) and mri = mr.(i) in
      for j = 0 to last_col do mri.(j) <- inner_mult last_row 0 m1i m2 j 
      done;
  done;;

let print_matrix m rows cols =
    for i=0 to (rows - 1) do
        for j=0 to (cols - 1) do
            print_string ((string_of_int m.(i).(j))^" ");
        done;
        print_newline ();
    done;
print_newline ();;

let print_array row =
    let size = Array.length row in
    for i=0 to size-1 do
        print_string ((string_of_int row.(i))^ " ");
    done;
    print_newline ();;

let row_mult row matrix col_number =
    let size = Array.length row in
    inner_mult (size-1) 0 row matrix col_number;;
        
let par_mult (m1,r1,c1) (m2,r2,c2) mres = 
    let my_role = Ftol.init () 
    and my_rank = Ftol.my_rank ()
    and nbrofprocesses  = Ftol.numberofprocesses () in
    let numberofslaves = nbrofprocesses - 1 in
    begin match my_role with
    | Ftol.Master ->
            (*send the initial matrices to the slaves*)
            let rows_to_send = int_of_float (ceil ((float_of_int r1)/.(float_of_int
            numberofslaves))) in
            for n = 1 to numberofslaves do 
                Ftol.send (Ftol.int_of_rank my_rank) (m2,r2,c2)
                (Ftol.rank_of_int n);
                print_string ("master sent m2 to slave rank:" ^ (string_of_int n));
                print_newline ();
                let increment = (n*rows_to_send - 1) in
                for i = (0+((n-1)*rows_to_send)) to (min increment (r1-1)) do
                    let m1i = m1.(i) in 
                    Ftol.send 0 (i,m1i) (Ftol.rank_of_int n);
                    print_string ("master sent row"^(string_of_int i)^" to slave rank:"
                    ^ (string_of_int n));
                    print_newline ();
                done;
            done;          
            
           (*get back the results of the computation*)
           for n = 1 to numberofslaves do 
               let increment = (n*rows_to_send - 1) in
               for i = (0+((n-1)*rows_to_send)) to (min increment (r1-1)) do
                   let boolval,tag,rank = Ftol.prob n (Ftol.rank_of_int n) in
                   if boolval then begin
                       let rownumber, resrow = Ftol.recv tag rank in
                       print_string ("the master has recieved row number:"
                       ^(string_of_int rownumber)); print_newline ();
                       for j=0 to c2-1 do
                           mres.(rownumber).(j) <- resrow.(j)
                       done;
                   end;
               done; 
            done;

            (*print the resulting matrix*)
            print_string ("the resulting matrix is:"); print_newline ();
            print_matrix mres r1 c2;
            
            for n = 1 to numberofslaves do 
                Ftol.send 0 (Pervasives.max_int,mres) (Ftol.rank_of_int n);
                print_string ("Master sent the slave number:"^(string_of_int
                n)^" a message to quit!"); print_newline ();
            done;
                            
    |Ftol.Slave ->
            (*receive the matrices from the master*)
            let m_2, r_2, c_2 = Ftol.recv 0 (Ftol.rank_of_int 0) in
            print_string ("slave rank:" ^ (string_of_int (Ftol.int_of_rank
            my_rank)) ^ " have recieved the second matrix"); print_newline ();
         (*   print_matrix m_2 r_2 c_2;*)
            
            let looping = ref 1 in 
            while !looping > 0  do
                (*first probe and then recieve the message.*)
                let it_came, tag, rank = Ftol.prob 0 (Ftol.rank_of_int 0) in 
                if it_came = true then begin
                    let row_number, m1row = Ftol.recv tag rank in
                    (*if the message contains the pervasives.max_int this is a
                    * quit message, so the slave sets the looping parameter to 0
                    * so that it quits the while loop.*)
                    if row_number = Pervasives.max_int then 
                        begin
                        looping := 0;
                    end else begin
                        print_string ("slave rank:" ^ (string_of_int
                        (Ftol.int_of_rank my_rank)) ^ 
                        " have recieved the row number"^ (string_of_int
                        row_number)); 
                        print_newline (); print_array m1row;  
                        (*begin calculating the result*)
                        let resrow = Array.create (Array.length m1row) 0 in
                        for j=0 to c_2 - 1 do
                            resrow.(j) <- row_mult m1row m_2 j;
                        done;
                        (*send the result back to the master*)
                        
                        Ftol.send (Ftol.int_of_rank my_rank) (row_number, resrow) 
                        (Ftol.rank_of_int 0);
                        print_string ("slave rank:"^ (string_of_int
                        (Ftol.int_of_rank my_rank)) ^ " sent the resulting row number "^
                        (string_of_int row_number)); 
                        print_newline (); print_array resrow;
                        print_string ("back to master"); print_newline ();
                    end;
                end;
            done;
    end;
    Mpi.finalize ()
;;
   
let matrixmult_program () =
    let rows = 3
    and cols = 3 in
    let m1,r1,c1 = mkmatrix rows cols 5
    and m2,r2,c2 = mkmatrix rows cols 10
    and mr = Array.make_matrix rows cols 0 in 
    mmult rows cols m1 m2 mr;
    print_matrix m1 rows cols; 
    print_matrix m2 rows cols; 
    print_matrix mr rows cols;; 

let second_program () =
    let rows = 4
    and cols1 = 4 
    and cols2 = 4 in    
    let m1,r1,c1 = mkmatrix rows cols1 5
    and m2,r2,c2 = mkmatrix cols1 cols2 10 
    and mres = Array.make_matrix rows cols2 0 in 
    par_mult (m1,r1,c1) (m2,r2,c2) mres;;
   
second_program ();;
