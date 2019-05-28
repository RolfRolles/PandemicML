type var = int
type binop = | Add | Mul

type expr = 
| Constant of int32
| Binop of expr * binop * expr
| Variable of var

type stmt =
| Assign of var * expr
| Print of expr

type bc = 
| Add2
| Mul2
| Load of int
| Store of int
| Const of int32
| PrintTop

let trans l_stmt = 
  let vars = Hashtbl.create 10 in
  let code = ref [] in let gen i = code := i::(!code) in
  let rec trans_expr = function
  | Binop(l,Add,r) -> trans_expr l; trans_expr r; gen Add2
  | Binop(l,Mul,r) -> trans_expr l; trans_expr r; gen Mul2
  | Variable(i)    -> Hashtbl.replace vars i (); gen (Load i)
  | Constant(c)    -> gen (Const c)
  in
  let rec trans_stmt = function
  | Assign(i,e) -> Hashtbl.replace vars i (); trans_expr e; gen (Store i)
  | Print(e) -> trans_expr e; gen PrintTop
  in
  List.iter trans_stmt l_stmt;
  let len,list = Hashtbl.fold (fun k _ (i,l) -> (i+1,k::l)) vars (0,[]) in
  let new_ht = Hashtbl.create (len*2) in 
  List.iter (fun i -> Hashtbl.replace new_ht i 0l) list;
  (List.rev !code,new_ht)

exception       UnboundVariable of var

let interp l_bc context = 
  let lookup v = 
     try Hashtbl.find context v 
     with Not_found -> raise (UnboundVariable v)
  in
  let stack = Stack.create () in
  let push x = Stack.push x stack in let pop () = Stack.pop stack in
  let interp = function
  | Add2     -> push (Int32.add (pop ()) (pop ()))
  | Mul2     -> push (Int32.mul (pop ()) (pop ()))
  | Const(c) -> push c
  | Load(i)  -> push (lookup i)
  | Store(i) -> Hashtbl.replace context i (pop ())
  | PrintTop -> Printf.printf "%ld\n" (pop ())
  in
  List.iter interp l_bc

let _ =
  let l_stmt = [
    Assign(0,Constant(2l));
    Assign(1,Constant(3l));
    Assign(2,Binop(Variable(0),Add,Variable(1)));
    Print(Variable(2));
    Assign(3,Binop(Variable(0),Mul,Variable(1)));
    Print(Variable(3))]
  in
  let bc,ht = trans l_stmt in
  interp bc ht
  
