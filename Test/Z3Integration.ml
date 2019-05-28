(* Z3 Integration *)
module Z3 = Z3.V3

type blah = 
{
  ctx: Z3.context;
  sort: X86Predicate.esize -> Z3.sort;
  ht_z3ast_of_flag: (X86.x86_flags,Z3.ast) Hashtbl.t;
  ht_z3ast_of_r32:  (X86.x86_reg32,Z3.ast) Hashtbl.t;
  ht_z3ast_of_r16:  (X86.x86_reg16,Z3.ast) Hashtbl.t;
  ht_z3ast_of_r8:   (X86.x86_reg8 ,Z3.ast) Hashtbl.t;
  z3true:  Z3.ast;
  z3false: Z3.ast;
}

let mk_f_sort ctx = 
  let s1  = Z3.mk_bv_sort ctx  1 in
  let s8  = Z3.mk_bv_sort ctx  8 in
  let s16 = Z3.mk_bv_sort ctx 16 in
  let s32 = Z3.mk_bv_sort ctx 32 in
  let open X86Predicate in
  function
  | S1  -> s1
  | S8  -> s8
  | S16 -> s16
  | S32 -> s32

let l_regs = 
  let open X86 in
  [Eax,Ax,Some(Ah,Al);
   Ecx,Cx,Some(Ch,Cl);
   Edx,Dx,Some(Dh,Dl);
   Ebx,Bx,Some(Bh,Bl);
   Esp,Sp,None;
   Ebp,Bp,None;
   Esi,Si,None;
   Edi,Di,None;]

let l_flags = 
  let open X86 in
  [X86F_C;
   X86F_P;
   X86F_A;
   X86F_S;
   X86F_Z;
   X86F_O;
   X86F_D;]

let mk_blah ctx f_sort z3true z3false s_pre s_post =
  let ht32 = Hashtbl.create 17 in
  let ht16 = Hashtbl.create 17 in
  let ht8  = Hashtbl.create 17 in
  let ht1  = Hashtbl.create 17 in
  let s32  = f_sort X86Predicate.S32 in
  let mk_var ctx name ty = Z3.mk_const ctx (Z3.mk_string_symbol ctx name) ty in

  List.iter (fun (r32,r16,r8p_opt) -> 
    let name = s_pre^X86Disasm.string_of_x86_reg32 r32^s_post in
    let v32 = mk_var ctx name s32 in
    Hashtbl.replace ht32 r32 v32;
    let v16 = Z3.mk_extract ctx 15 0 v32 in
    Hashtbl.replace ht16 r16 v16;
    match r8p_opt with
    | Some(h8,l8) ->      let vh8 = Z3.mk_extract ctx 15 8 v32 in
      let vl8 = Z3.mk_extract ctx  7 0 v32 in
      Hashtbl.replace ht8 h8 vh8;
      Hashtbl.replace ht8 l8 vl8
    | None -> ())
    l_regs;
  let s1  = f_sort X86Predicate.S1 in
  List.iter (fun f -> 
    let name = s_pre^X86Disasm.string_of_x86_flags f^s_post in
    let vf = mk_var ctx name s1 in
    Hashtbl.replace ht1 f vf)
    l_flags;
  {
    ctx = ctx;
    sort = f_sort;
    ht_z3ast_of_r32  = ht32;
    ht_z3ast_of_r16  = ht16;
    ht_z3ast_of_r8   = ht8;
    ht_z3ast_of_flag = ht1;
    z3true = z3true;
    z3false = z3false;
  }

type blah2 =
{
  input: blah;
  output1: blah;
  output2: blah;
}

let mk_blah2 l_params = 
  (* V4 API
  let ctx     = Z3.mk_context l_params in
  *)
  let ctx     = Z3.mk_context_x (Array.of_list l_params) in
  let f_sort  = mk_f_sort ctx in
  let z3true  = Z3.mk_int64 ctx 1L (f_sort X86Predicate.S1) in
  let z3false = Z3.mk_int64 ctx 0L (f_sort X86Predicate.S1) in
  let f = mk_blah ctx f_sort z3true z3false in
  {
    input   = f "IN_" "";
    output1 = f "out_" "_1";
    output2 = f "out_" "_2";
  }
  
let z3ast_of_expr z3s expr =
  let ctx = z3s.ctx in
  (* Z3 uses an internal representation of booleans that is not synonymous with
     the numbers 1 and 0.  Therefore we use this function to build an expression
     that converts Z3's booleans to numbers. *)
  let z3constify i32 sort = Z3.mk_int64 ctx (Int64.of_int32 i32) sort in
  let z3boolify      expr = Z3.mk_ite ctx expr z3s.z3true z3s.z3false in
  let z3boolify_neg  expr = Z3.mk_ite ctx expr z3s.z3false z3s.z3true in
  let mk_z3bool      expr = Z3.mk_ite ctx (Z3.mk_eq ctx expr z3s.z3true) (Z3.mk_true ctx) (Z3.mk_false ctx) in
  let z3bit expr n        = Z3.mk_extract ctx n n expr in
  let open X86Predicate in
  let rec aux = function
  | X86Flag(f)        -> (Hashtbl.find z3s.ht_z3ast_of_flag f,S1)
  | X86Reg(X86.Gd(r)) -> (Hashtbl.find z3s.ht_z3ast_of_r32  r,S32)
  | X86Reg(X86.Gw(r)) -> (Hashtbl.find z3s.ht_z3ast_of_r16  r,S16)
  | X86Reg(X86.Gb(r)) -> (Hashtbl.find z3s.ht_z3ast_of_r8   r,S8)
  | BitImm(b)         -> (if b then z3s.z3true else z3s.z3false),S1
  | Imm(X86.Id(i32))  -> (z3constify i32 (z3s.sort S32),S32)
  | Imm(X86.Iw(i32))  -> (z3constify i32 (z3s.sort S16),S16)
  | Imm(X86.Ib(i32))  -> (z3constify i32 (z3s.sort S8) ,S8)
  | Binop(l,o,r)      -> 
    let z3l,ls = aux l in
    let z3r,rs = aux r in
    let d f = f ctx z3l z3r,ls in
    let e f = f ctx z3l z3r in
    let s f = f ctx z3l (Z3.mk_zero_ext ctx (int_of_esize ls - int_of_esize rs) z3r),ls in
   (match o with
    | Add -> d Z3.mk_bvadd
    | Sub -> d Z3.mk_bvsub
    | Xor -> d Z3.mk_bvxor
    | And -> d Z3.mk_bvand
    | Or  -> d Z3.mk_bvor
    | Shl -> s Z3.mk_bvshl
    | Shr -> s Z3.mk_bvlshr
    | Sar -> s Z3.mk_bvashr
    | Eq  -> z3boolify (e Z3.mk_eq)    ,S1
    | Ne  -> z3boolify_neg (e Z3.mk_eq),S1
    | Slt -> z3boolify (e Z3.mk_bvslt) ,S1
    | Sle -> z3boolify (e Z3.mk_bvsle) ,S1
    | Ult -> z3boolify (e Z3.mk_bvult) ,S1
    | Ule -> z3boolify (e Z3.mk_bvule) ,S1)
  | Unop(o,e) ->
    let ze,es = aux e in
   (match o with
    | Inc -> Z3.mk_bvadd ctx ze (z3constify 1l (z3s.sort es)),es
    | Dec -> Z3.mk_bvsub ctx ze (z3constify 1l (z3s.sort es)),es
    | Not -> Z3.mk_bvnot ctx ze,es
    | Neg -> Z3.mk_bvneg ctx ze,es)
  | Bitop(o,e) ->
    let ze,es = aux e in
   (match o with
    | Parity    -> 
      let bits = List.map (z3bit ze) [0;1;2;3;4;5;6;7] in
      let expr = 
        Z3.mk_bvnot 
          ctx 
         (List.fold_left 
           (fun p b -> Z3.mk_bvxor ctx p b) 
           (List.hd bits) 
           (List.tl bits)) 
      in
      expr,S1
    | SignBit   -> z3bit ze (X86Predicate.int_of_esize es-1),S1
    | SecondBit -> z3bit ze (X86Predicate.int_of_esize es-2),S1
    | FifthBit  -> z3bit ze 4,S1)
  | Extend(m,s,e) ->
    let ze,es = aux e in
   (match m with
    (* Cast to a larger size *)
    | Unsigned -> (Z3.mk_zero_ext ctx (int_of_esize s - int_of_esize es) ze,s)
    | Signed   -> (Z3.mk_sign_ext ctx (int_of_esize s - int_of_esize es) ze,s)
    (* Cast to a smaller size *)
    | Low      -> (Z3.mk_extract  ctx (int_of_esize s - 1) 0 ze,s))
  | ITE(b,t,f) ->
    let be,bs = aux b in
    let te,ts = aux t in
    let fe,fs = aux f in
    Z3.mk_ite ctx (mk_z3bool be) te fe,ts    
  in
  let z3ast,_ = aux expr in
(*Printf.printf "%s -> %s\n" (string_of_expr expr) (Z3.ast_to_string ctx z3ast);*)
  z3ast

let z3ast_of_stmt z3s_in z3s_out stmt = 
  let ctx = z3s_in.ctx in
  let z3constify i32 sort = Z3.mk_int64 ctx (Int64.of_int32 i32) (z3s_in.sort sort) in
  let lookup_out ht x e = 
    let fv = Hashtbl.find ht x in
    let ez = z3ast_of_expr z3s_in e in
    Z3.mk_eq z3s_in.ctx fv ez
  in
  let open X86Predicate in
  let rec aux = function
  | FlagEquals(f,e)          -> lookup_out z3s_out.ht_z3ast_of_flag f e    
  | RegEquals(X86.Gd(r32),e) -> lookup_out z3s_out.ht_z3ast_of_r32 r32 e
  | RegEquals(X86.Gw(r16),e) -> lookup_out z3s_out.ht_z3ast_of_r16 r16 e
  | RegEquals(X86.Gb(r8),e)  -> lookup_out z3s_out.ht_z3ast_of_r8  r8  e
  | ImmEquals(X86.Id(i32),e) -> Z3.mk_eq z3s_in.ctx (z3ast_of_expr z3s_in e) (z3constify i32 S32)
  | ImmEquals(X86.Iw(i32),e) -> Z3.mk_eq z3s_in.ctx (z3ast_of_expr z3s_in e) (z3constify i32 S16)
  | ImmEquals(X86.Ib(i32),e) -> Z3.mk_eq z3s_in.ctx (z3ast_of_expr z3s_in e) (z3constify i32 S8)
  in 
  let z3ast = aux stmt in
(*Printf.printf "%s -> %s\n" (string_of_stmt stmt) (Z3.ast_to_string ctx z3ast);*)
  z3ast

(*
  let _ = Printf.printf "6\n%!" in
  let _ = Printf.printf "7\n%!" in
  let _ = Printf.printf "8\n%!" in
  let _ = Printf.printf "9\n%!" in
  let _ = Printf.printf "A\n%!" in
    let _ = Printf.printf "B\n%!" in
    let _ = Printf.printf "C\n%!" in
    let _ = Printf.printf "D\n%!" in
*)
let z3time = ref 0.0

let solve_distinguishment ctx s1 s2 v1 v2 =
  let s = Z3.mk_and ctx [|(Z3.mk_not ctx (Z3.mk_eq ctx v1 v2));s1; s2;|] in
(* V4 API
  let solver = Z3.mk_solver_for_logic ctx (Z3.mk_string_symbol ctx "QF_BV") in
  let _ = Z3.solver_push ctx solver in
  let _ = Z3.solver_assert ctx solver s in
  let lb = Z3.solver_check ctx solver in
*)
  let _ = Z3.push ctx in
  Z3.assert_cnstr ctx s;
  let t = Sys.time () in
  let sctx = Z3.context_to_string ctx in
  let (result,m) = Z3.check_and_get_model ctx in
  z3time := !z3time +. (Sys.time () -. t);
  let string_of_search_failure = function 
  |	Z3.NO_FAILURE       -> "NO_FAILURE"
  |	Z3.UNKNOWN          -> "UNKNOWN"
  |	Z3.TIMEOUT          -> "TIMEOUT"
  |	Z3.MEMOUT_WATERMARK -> "MEMOUT_WATERMARK"
  |	Z3.CANCELED         -> "CANCELED"
  |	Z3.NUM_CONFLICTS    -> "NUM_CONFLICTS"
  |	Z3.THEORY           -> "THEORY"
  |	Z3.QUANTIFIERS      -> "QUANTIFIERS"
  in
  
  let res = match result with
  | Z3.L_FALSE ->
  (*let _ = Printf.printf "Distinguishment assertion was unsatisfiable\n" in*)
    []
  | Z3.L_TRUE ->
    let stmt_of_model_data svar sv32 l =
      let v32  = Int64.to_int32 (Int64.of_string sv32) in
      let v1   = if Int32.logand 1l v32 <> 0l then true else false in
      let open X86Predicate in
      match svar with
      | "eax" -> (RegEquals(X86.Gd(X86.Eax),Imm(X86.Id(v32)))::l)
      | "ebx" -> (RegEquals(X86.Gd(X86.Ebx),Imm(X86.Id(v32)))::l)
      | "ecx" -> (RegEquals(X86.Gd(X86.Ecx),Imm(X86.Id(v32)))::l)
      | "edx" -> (RegEquals(X86.Gd(X86.Edx),Imm(X86.Id(v32)))::l)
      | "esp" -> (RegEquals(X86.Gd(X86.Esp),Imm(X86.Id(v32)))::l)
      | "ebp" -> (RegEquals(X86.Gd(X86.Ebp),Imm(X86.Id(v32)))::l)
      | "esi" -> (RegEquals(X86.Gd(X86.Esi),Imm(X86.Id(v32)))::l)
      | "edi" -> (RegEquals(X86.Gd(X86.Edi),Imm(X86.Id(v32)))::l)
      | "CF"  -> (FlagEquals(X86.X86F_C,BitImm(v1))::l)
      | "PF"  -> (FlagEquals(X86.X86F_P,BitImm(v1))::l)
      | "AF"  -> (FlagEquals(X86.X86F_A,BitImm(v1))::l)
      | "SF"  -> (FlagEquals(X86.X86F_S,BitImm(v1))::l)
      | "ZF"  -> (FlagEquals(X86.X86F_Z,BitImm(v1))::l)
      | "OF"  -> (FlagEquals(X86.X86F_O,BitImm(v1))::l)
      | "DF"  -> (FlagEquals(X86.X86F_D,BitImm(v1))::l)
      | _     ->
        Printf.printf "Unknown constant %s\n" svar (*  = %s (Z3.get_numeral_string ctx a)*); l
    in
    let eat str amt =
      let len = String.length str in
      let sub = String.sub str 0 amt in
      let rst = String.sub str amt (len-amt) in
      (sub,rst)
    in
    let scan str str2 =
      let len  = String.length str  in
      let len2 = String.length str2 in
      let rec aux i =
        if i = len then None else
        if i+len2 < len && String.sub str i len2 = str2 then Some(snd(eat str (i+len2))) else 
        aux (i+1)
      in aux 0
    in
    let take_nums str = 
      let len = String.length str in
      let rec aux list i =
        if i = len
        then String.concat "" (List.rev list),""
        else 
          match str.[i] with
          | '0'..'9' as x -> aux ((Printf.sprintf "%c" x)::list) (i+1)
          | _ -> String.concat "" (List.rev list),String.sub str i (len-i)
      in
      aux [] 0
    in  
    let rec consume str list = 
      let _ = () in
      if String.length str = 0 then list else
      let beg,str = eat str 3 in
      match beg with
      | "out" -> 
      (match scan str "\n" with
        | Some(str) -> consume str list
        | None -> list)
      | "IN_" -> 
      (*let _ = Printf.printf "Parsing in statement: %s\n" str in*)
        let reg,str = eat str 3 in
        let regstr = 
          match reg.[0],reg.[1] with
          | 'e',_ -> reg
          | _,'F' -> String.sub reg 0 2
          | _,_ -> failwith "Z3 parsing regname failed"
        in
       (match scan str "bv" with
        | Some(str) -> 
          let num,str = take_nums str in
         (match str.[0] with
          | '[' -> 
            let _,str = eat str 1 in
            let _,str = take_nums str in
           (match str.[0] with 
            | ']' -> 
              let _,str = eat str 1 in
             (match scan str "\n" with
              | Some(str) -> consume str (stmt_of_model_data regstr num list)
              | None -> stmt_of_model_data regstr num list)
            | _ -> failwith "Z3 parsing BV size 2 failed")
          | _ -> failwith "Z3 parsing BV size 1 failed")
        | None -> failwith "Z3 parsing BV failed")
      | s -> failwith ("Z3 parsing line begin failed: "^s)
    in
    let rmod = Z3.model_to_string ctx m in 
    let res = consume rmod [] in
    Z3.del_model ctx m; 
  (*Printf.printf "Context: %s\nModel: %s\nAST: %s\n" sctx rmod (Z3.ast_to_string ctx s);*)
    res
    
    (* V4 API
    let model  = Z3.solver_get_model ctx solver in
    let rmodel = Z3.model_refine ctx model in
    let _ = Z3.solver_reset ctx solver in
    Hashtbl.fold 
     (fun f a l -> 
        let name = Z3.get_symbol_string ctx (Z3.get_decl_name ctx f) in
        let slen = String.length name in
        if slen >= 3 && String.sub name 0 3 = "IN_"
        then
          let svar = String.sub name 3 (slen-3) in
          let sv32 = Z3.get_numeral_string ctx a in
          stmt_of_model_data svar sv32 l
        else l)
     (let open Z3 in rmodel.consts)
      []
    *)
    
  | Z3.L_UNDEF ->
    let _ = Printf.printf "Z3 error/timeout: %s\n" (string_of_search_failure (Z3.get_search_failure ctx)) in
    []
  in
  (* V4 API 
  let _ = Z3.solver_pop ctx solver in 
  *)
  (* V3 API *)
  let _ = Z3.pop ctx 1 in
  res


(*
  let _ = Printf.printf "1\n%!" in
  let _ = Printf.printf "2\n%!" in
  let _ = Printf.printf "3\n%!" in
  let _ = Printf.printf "4\n%!" in
  let _ = Printf.printf "5\n%!" in
*)
let distinguish_flag t f e1 e2 =
  let open X86Predicate in
  let s1 = z3ast_of_stmt t.input t.output1 (FlagEquals(f,e1)) in
  let s2 = z3ast_of_stmt t.input t.output2 (FlagEquals(f,e2)) in
  let v1 = Hashtbl.find t.output1.ht_z3ast_of_flag f in
  let v2 = Hashtbl.find t.output2.ht_z3ast_of_flag f in
  solve_distinguishment t.input.ctx s1 s2 v1 v2

let distinguish_reg32 t f e1 e2 =
  let open X86Predicate in
  let s1 = z3ast_of_stmt t.input t.output1 (RegEquals(X86.Gd(f),e1)) in
  let s2 = z3ast_of_stmt t.input t.output2 (RegEquals(X86.Gd(f),e2)) in
  let v1 = Hashtbl.find t.output1.ht_z3ast_of_r32 f in
  let v2 = Hashtbl.find t.output2.ht_z3ast_of_r32 f in
  solve_distinguishment t.input.ctx s1 s2 v1 v2

let distinguish_reg16 t f e1 e2 =
  let open X86Predicate in
  let s1 = z3ast_of_stmt t.input t.output1 (RegEquals(X86.Gw(f),e1)) in
  let s2 = z3ast_of_stmt t.input t.output2 (RegEquals(X86.Gw(f),e2)) in
  let v1 = Hashtbl.find t.output1.ht_z3ast_of_r16 f in
  let v2 = Hashtbl.find t.output2.ht_z3ast_of_r16 f in
  solve_distinguishment t.input.ctx s1 s2 v1 v2

let distinguish_reg8 t f e1 e2 =
  let open X86Predicate in
  let s1 = z3ast_of_stmt t.input t.output1 (RegEquals(X86.Gb(f),e1)) in
  let s2 = z3ast_of_stmt t.input t.output2 (RegEquals(X86.Gb(f),e2)) in
  let v1 = Hashtbl.find t.output1.ht_z3ast_of_r8 f in
  let v2 = Hashtbl.find t.output2.ht_z3ast_of_r8 f in
  solve_distinguishment t.input.ctx s1 s2 v1 v2
