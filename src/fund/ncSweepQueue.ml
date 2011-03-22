
open NcPrelude

module D = BatDllist

exception Empty

type 'a state = {
    mutable head : 'a D.t;
    mutable sweep : 'a D.t;
}

type 'a t = {
    alive : 'a -> bool;
    mutable state : 'a state option
}

let sweep q = match q.state with
    | None -> ()
    | Some s ->
	let node = s.sweep in
	s.sweep <- D.prev node;
	if not (q.alive (D.get node)) then begin
	    if node == s.head then begin
		let next = D.next node in
		if node == next
		then q.state <- None
		else s.head <- next
	    end;
	    D.remove node
	end
	    

let create alive = { alive; state = None }

let push v q = match q.state with 
    | None -> 
	let node = D.create v in
	q.state <- Some { head = node; sweep = node };
	sweep q
    | Some s -> 
	ignore (D.prepend s.head v);
	sweep q

    let head = s.head in
    

let tryPop q = match q.state with
    | None -> None
    | Some s -> 
	let head = s.head in
	let tail = D.prev s.head in
	let node = ref head in
	let next_live node =
	    if q.alive (D.get node)
	    then node
	    else if node == tail then begin
		q.state <- None;
		None
	    end else
		next_live (D.next node)
	    
	    
		
	    
	    
	    
	    if q.alive head
	
	let oldhead = s.head in
	s.head <- D.next oldhead;
	D.remove oldhead;
	if s.head == oldhead 
	then q.state <- None
	else if s.sweep == oldhead
	then s.sweep <- D.prev s.head
	else sweep q;
	Some (D.get oldhead)

let pop q = match tryPop q with
    | None -> raise Empty
    | Some v -> v
