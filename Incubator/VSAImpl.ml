let make_top s = ReadableVSA.make_top (IRUtil.bits s)
let si_of_const i = let open ReadableVSA in { lb = i; ub = i; s = 0L }

let rec valueset_of_expr ctxt = function
| Var(Mem(i,e,s)) -> 
| Var(Variable(i,s)) ->
| Const(i,s) -> si_of_const i
| Cast(c,s,e) ->
  let vse = valueset_of_expr ctxt e in
 (match c with
  | Unsigned -> failwith "Unsigned cast, implement (shift values and reduce)"
  | Signed   -> failwith "Signed cast, implement (do nothing, already represented by a signed quantity)"
  | High     -> failwith "High cast, implement (shift values)"
  | Low      -> failwith "Low cast, implement")
| Binop(l,o,r) ->
  let vsl = valueset_of_expr ctxt l in
  let vsr = valueset_of_expr ctxt r in
 (match o with
  | Add -> ReadableVSA.add_SIs ? vsl vsr
  | Sub 
  | Mul 
  | SDiv 
  | UDiv 
  | SMod 
  | UMod 
  | Shl 
  | Shr 
  | Sar 
  | And 
  | Or 
  | Xor 
  | EQ 
  | NE 
  | ULT 
  | ULE 
  | SLT 
  | SLE
  )


| Binop of expr * binop * expr 
| Unop of unop * expr 
| Load of expr * expr * typereg (** Memory, address, size *)
| Store of expr * expr * expr * typereg (** Memory, address, expr to write, size *)
| Var of var
| Let of var * expr * expr (** Let var = expr1 in expr2 *)
| Cast of castkind * typereg * expr (** Cast({!castkind},size,expr) *)
| Const of const
