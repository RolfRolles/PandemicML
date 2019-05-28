exception SyntacticallyUnsatisfiable

(* Formula might have constants in it.  We use this to represent a formula as 
   we are translating it, and then the simplification phase removes the ConTrue
   and ConFalse type constructors to produce ... *)
type intermediate_literal =
| Var of string
| NegVar of string
| ConTrue
| ConFalse

(* ... the final literal representation, which has no constants. *)
type literal = 
| Variable of string
| NegVariable of string

(* New intermediate variable producer *)
let f_newvar_with_prefix prefix =
  let counter = ref 0 in
 (fun () -> 
    let i = !counter in
    incr counter;
    Var(prefix^string_of_int i))

(* Specialized for the Tseitin transform *)
let tseitin_newvar = f_newvar_with_prefix "@tv#"

(* Unordered append, tail-recursive *)
let cat =
  let rec aux outlist = function
  | x::xs -> aux (x::outlist) xs
  | [] -> outlist
  in aux

(* From ListUtil *)
let map_filter f_opt list =
  List.fold_left (fun acc el -> match (f_opt el) with | Some(x) -> (x::acc) | None -> acc) [] (List.rev list)

let tseitin f = 
  let open PL in
  let newvar = tseitin_newvar in
  
  (* Negate a literal; incorporates several simplifications already *)
  let neglit = function
  | Var(s) -> NegVar(s)
  | NegVar(s) -> Var(s)
  | ConTrue -> ConFalse
  | ConFalse -> ConTrue
  in
  
  (* Simplifier.  Re-think this. *)
  let simpl l_disjuncts =
    let rec aux outlist = function
    | [] when outlist = [] -> raise SyntacticallyUnsatisfiable
    | [] -> Some(outlist)
    | (ConTrue)::_ -> None
    | (ConFalse)::xs -> aux outlist xs
    | x::xs -> aux (x::outlist) xs
    in
    aux [] l_disjuncts
  in
  
  (* Tseitin transformer. *)
  let rec aux = function
  | Constant(True)   -> (ConTrue,  [])
  | Constant(False)  -> (ConFalse, [])
  | Not(Variable(p)) -> (NegVar(p),[])
  | Variable(p)      -> (Var(p),   [])
  | And(f1,f2) ->
    let (f1_rep,f1_enc),(f2_rep,f2_enc),rep = aux f1,aux f2,newvar () in
   (rep,
    [neglit rep;f1_rep]::
    [neglit rep;f2_rep]::
    [rep;neglit f1_rep;neglit f2_rep]::
   (cat f1_enc f2_enc))
  | Or(f1,f2) ->
    let (f1_rep,f1_enc),(f2_rep,f2_enc),rep = aux f1,aux f2,newvar () in
   (rep,
    [rep;neglit f1_rep]::
    [rep;neglit f2_rep]::
    [neglit rep;f1_rep;f2_rep]::
   (cat f1_enc f2_enc))
  | Not(f) ->
    let (f_rep,f_enc),rep = aux f,newvar () in
   (rep,
    [neglit rep;neglit f_rep]::
    [rep;f_rep]::
    f_enc)
  | Implies(f1,f2) ->
    let (f1_rep,f1_enc),(f2_rep,f2_enc),rep = aux f1,aux f2,newvar () in
   (rep,
    [rep;f1_rep]::
    [rep;neglit f2_rep]::
    [neglit rep;neglit f1_rep;f2_rep]::
   (cat f1_enc f2_enc))
  | Iff(f1,f2) ->
    let (f1_rep,f1_enc),(f2_rep,f2_enc),rep = aux f1,aux f2,newvar () in
   (rep,
    [neglit rep;neglit f1_rep;f2_rep]::
    [neglit rep;f1_rep;neglit f2_rep]::
    [rep;neglit f1_rep;neglit f2_rep]::
    [rep;f1_rep;f2_rep]::
   (cat f1_enc f2_enc))
  in map_filter simpl (snd(aux f))

let string_of_il = function
| Var(s)    -> s
| NegVar(s) -> "!"^s
| ConTrue   -> "true"
| ConFalse  -> "false"

let string_of_clause =
  let rec aux outlist = function
  | []    -> "Empty clause, unsatisfiable!"
  | x::[] -> "("^(List.fold_left (^) "" (List.rev (x::outlist)))^")"
  | x::xs -> aux (" || "::el::parts) xs
  in aux []

let string_of_cnf =
  let rec aux outlist = function
  | [] -> "Empty formula, valid!"
  | x::[] -> List.fold_left (^) "" (List.rev (x::outlist))

type intermediate_literal =
| Var of string
| NegVar of string
| ConTrue
| ConFalse
