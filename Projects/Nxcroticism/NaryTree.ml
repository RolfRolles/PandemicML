type ('k,'d,'v) nary_tree = | Node of 'k * 'd * ('v,unit) Hashtbl.t * ('k,('k,'d,'v) nary_tree) Hashtbl.t
type ('k,'d,'v) nary_tree_roots = ('k,('k,'d,'v) nary_tree) Hashtbl.t
type ('k,'d,'v) t = { roots: ('k,'d,'v) nary_tree_roots; } (* ' *)
let make  () = { roots = Hashtbl.create 23; }
let make_n n = { roots = Hashtbl.create n; }

let ht_find_opt h a =
  let res =
    try  Some(Hashtbl.find h a)
    with Not_found -> None
  in res

(* Expects reversed lists *)
let insert t list =
  let rec insert node_ht = function
  | (a,i,ir)::airs ->
   (match ht_find_opt node_ht i with
    | Some(Node(_,_,addr_ht,child_ht)) ->
      Hashtbl.replace addr_ht a ();
      insert child_ht airs
    | None ->
      let new_address = Hashtbl.create 7 in
      Hashtbl.replace new_address a ();
      let new_child = Hashtbl.create 7 in
      Hashtbl.replace node_ht i (Node(i,ir,new_address,new_child));
      insert new_child airs)
  | [] -> ()
  in
  match list with
  | [] -> ()
  | _ -> insert t.roots list

let rec filter old_node_ht new_node_ht = 
  Hashtbl.iter (fun i x -> match x with 
  | Node(instr,None,    addr_ht,child_ht) -> ()
  | Node(instr,Some(ir),addr_ht,child_ht) -> 
    let new_child_ht = Hashtbl.create (Hashtbl.length child_ht) in
    let _ = filter child_ht new_child_ht in
    Hashtbl.replace new_node_ht i (Node(instr,ir,addr_ht,new_child_ht)))
    old_node_ht
      
let filter_roots tree = 
  let new_roots = Hashtbl.create (Hashtbl.length tree.roots) in
  filter tree.roots new_roots;
  { roots = new_roots; }

