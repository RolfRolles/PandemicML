open FrameworkUtil

(* Returns Some(x) if ht[key] exists, None otherwise *)
let ht_find_opt ht key =
  let res =
    try Some(Hashtbl.find ht key)
    with Not_found -> None
  in res

(* For hash tables where the values are lists, add the k/v pair *)
let ht_add_to_value_list ht key value =
  match ht_find_opt ht key with
  | None    -> Hashtbl.replace ht key [value]
  | Some(l) -> Hashtbl.replace ht key (value::l)

(* Given an ('a,'b) hash table, make a ('b,'a list) hash table *)
let reverse_ht ht =
  let nht = Hashtbl.create (Hashtbl.length ht) in
  Hashtbl.iter (fun k v -> ht_add_to_value_list nht v k) ht;
  nht

(* Given a list ('a,int32), make an ('a,IR.expr) hash table w/Const(int32,s) values  *)
let new_ht s pairs = 
  let ht = Hashtbl.create (List.length pairs * 2) in
  List.iter (fun (x,y) -> Hashtbl.replace ht x (IRUtil.mk_fixed_const_of_i32 y s)) pairs;
  ht

(* Compute the intersection of two IR.var -> IR.VarSet maps *)
let varmapset_inter m1 m2 =
  let open IRUtil in
  let pm = VarMap.mapi
   (fun k s1 ->
      let res = 
        try Some(VarMap.find k m2)
        with Not_found -> None
      in match res with
      | None -> VarSet.empty
      | Some(s2) -> VarSet.inter s1 s2)
    m1
  in VarMap.filter (fun _ s -> not (VarSet.is_empty s)) pm

(* Remove those variables that are not contained within varset from the 
   context hash table*)
let remove_irtrans_vars varset ht =
  let vl = Hashtbl.fold 
   (fun k _ l -> 
      if IRUtil.VarSet.mem k varset
      then l
      else k::l)
    ht
    []
  in
  List.iter (Hashtbl.remove ht) vl;
  ht

(* Triplicate stuff *)
type 'a triplicate =
{
  val8: 'a;
  val16: 'a;
  val32: 'a;
}

let select_member s t = match s with
| IR.TypeReg_8  -> t.val8 
| IR.TypeReg_16 -> t.val16
| IR.TypeReg_32 -> t.val32
| _ -> invalid_arg "select_member"

let replace_member s t v = match s with
| IR.TypeReg_8  -> { t with val8  = v; }
| IR.TypeReg_16 -> { t with val16 = v; }
| IR.TypeReg_32 -> { t with val32 = v; }
| _ -> invalid_arg "replace_member"

let triplicate f x =
  { val8 = f IR.TypeReg_8 x; val16 = f IR.TypeReg_16 x; val32 = f IR.TypeReg_32 x }

let triplicate_map f x =
  { val8 = f IR.TypeReg_8 x.val8; val16 = f IR.TypeReg_16 x.val16; val32 = f IR.TypeReg_32 x.val32 }
  
let triplicate_iter f t =
  let _ = f IR.TypeReg_8  t.val8  in
  let _ = f IR.TypeReg_16 t.val16 in
  f IR.TypeReg_32 t.val32

let triplicate_depair { val8 = (a8,b8); val16 = (a16,b16); val32 = (a32,b32); } =
  { val8 = a8; val16 = a16; val32 = a32; },
  { val8 = b8; val16 = b16; val32 = b32; }
  
let print_triplicate f t =
  f t.val32;
  f t.val16;
  f t.val8

(* Fold each individually, return them separately *)
let triple_fold_triple f_outer f_inner acc_t t =
{ val8  = f_outer f_inner t.val8  acc_t.val8; 
  val16 = f_outer f_inner t.val16 acc_t.val16; 
  val32 = f_outer f_inner t.val32 acc_t.val32; }

let fold_triple f_outer f_inner acc t =
  let acc8  = f_outer f_inner t.val8  acc  in
  let acc16 = f_outer f_inner t.val16 acc8 in
  f_outer f_inner t.val32 acc16

(* NEEDS REFACTORING NEEDS REFACTORING NEEDS REFACTORING NEEDS REFACTORING *)
(* NEEDS REFACTORING NEEDS REFACTORING NEEDS REFACTORING NEEDS REFACTORING *)
(* NEEDS REFACTORING NEEDS REFACTORING NEEDS REFACTORING NEEDS REFACTORING *)
(* NEEDS REFACTORING NEEDS REFACTORING NEEDS REFACTORING NEEDS REFACTORING *)
(* NEEDS REFACTORING NEEDS REFACTORING NEEDS REFACTORING NEEDS REFACTORING *)
(* NEEDS REFACTORING NEEDS REFACTORING NEEDS REFACTORING NEEDS REFACTORING *)
(* NEEDS REFACTORING NEEDS REFACTORING NEEDS REFACTORING NEEDS REFACTORING *)

let int_of_binop = function
| IR.Add  -> 0
| IR.Sub  -> 1
| IR.Mul  -> 2
| IR.SDiv -> 3
| IR.UDiv -> 4
| IR.SMod -> 5
| IR.UMod -> 6
| IR.Shl  -> 7
| IR.Shr  -> 8
| IR.Sar  -> 9
| IR.And  -> 10
| IR.Or   -> 11
| IR.Xor  -> 12
| IR.EQ   -> 13
| IR.NE   -> 14
| IR.ULT  -> 15
| IR.ULE  -> 16
| IR.SLT  -> 17
| IR.SLE  -> 18

module BinopComparator = struct
  type t = IR.binop
  let compare b1 b2 = 
    Pervasives.compare (int_of_binop b1) (int_of_binop b2)
end

module BinopMap = Map.Make(BinopComparator)

module VarBinopVarComparator = struct
  type t = (IR.var * IR.binop * IR.var)
  let compare (l1,b1,r1) (l2,b2,r2) = 
    let open IR in
    let c1 = IRUtil.var_comparator l1 l2 in
    if c1 <> 0 then c1 else
    let c3 = IRUtil.var_comparator r1 r2 in
    if c3 <> 0 then c3 else Pervasives.compare (int_of_binop b1) (int_of_binop b2)
end

module VarBinopVarSet = Set.Make(VarBinopVarComparator)

module VarVarBinopVarComparator = struct
  type t = (IR.var * IR.var * IR.binop * IR.var)
  let compare (v1,l1,b1,r1) (v2,l2,b2,r2) = 
    let open IR in
    let c1 = IRUtil.var_comparator v1 v2 in
    if c1 <> 0 then c1 else
    let c1 = IRUtil.var_comparator l1 l2 in
    if c1 <> 0 then c1 else
    let c3 = IRUtil.var_comparator r1 r2 in
    if c3 <> 0 then c3 else Pervasives.compare (int_of_binop b1) (int_of_binop b2)
end

module VarVarBinopVarSet = Set.Make(VarVarBinopVarComparator)

module VarBinopVarInt32Comparator = struct
  type t = (IR.var * IR.binop * IR.var * int32)
  let compare (v11,b1,v12,i321) (v21,b2,v22,i322) = 
    let c1 = IRUtil.var_comparator v11 v21 in
    if c1 <> 0 then c1 else
    let c3 = IRUtil.var_comparator v12 v22 in
    if c3 <> 0 then c3 else
    let c2 = Int32.compare i321 i322 in
    if c2 <> 0 then c2 else Pervasives.compare (int_of_binop b1) (int_of_binop b2)
end

module VarBinopVarInt32Set = Set.Make(VarBinopVarInt32Comparator)

module VarBinopInt32Comparator = struct
  type t = (IR.var * IR.binop * int32)
  let compare (v11,b1,i321) (v21,b2,i322) = 
    let c1 = IRUtil.var_comparator v11 v21 in
    if c1 <> 0 then c1 else
    let c2 = Int32.compare i321 i322 in
    if c2 <> 0 then c2 else Pervasives.compare (int_of_binop b1) (int_of_binop b2)
end

module VarBinopInt32Set = Set.Make(VarBinopInt32Comparator)

(* Compute the intersection of two IR.var -> IR.VarBinopVarSet maps 
   Duplicated from before, figure out how to use first-class modules *)
let varmapvbvset_inter m1 m2 =
  let open IRUtil in
  let pm = VarMap.mapi
   (fun k s1 ->
      let res = 
        try Some(VarMap.find k m2)
        with Not_found -> None
      in match res with
      | None -> VarBinopVarSet.empty
      | Some(s2) -> VarBinopVarSet.inter s1 s2)
    m1
  in VarMap.filter (fun _ s -> not (VarBinopVarSet.is_empty s)) pm

module VarInt32Comparator = struct
  type t = (IR.var * int32)
  let compare (v1,d1) (v2,d2) = 
    let c1 = IRUtil.var_comparator v1 v2 in
    if c1 <> 0 then c1 else Int32.compare d1 d2
end

module VarInt32Set = Set.Make(VarInt32Comparator)

module VarVarComparator = struct
  type t = (IR.var * IR.var)
  let compare (v11,v12) (v21,v22) = 
    let c1 = IRUtil.var_comparator v11 v21 in
    if c1 <> 0 then c1 else IRUtil.var_comparator v12 v22
end

module VarVarSet = Set.Make(VarVarComparator)
module VarVarMap = Map.Make(VarVarComparator)

module VarVarInt32Comparator = struct
  type t = (IR.var * IR.var * int32)
  let compare (v11,v12,i321) (v21,v22,i322) = 
    let c1 = IRUtil.var_comparator v11 v21 in
    if c1 <> 0 then c1 else
    let c3 = IRUtil.var_comparator v12 v22 in
    if c3 <> 0 then c3 else
    Int32.compare i321 i322
end

module VarVarInt32Set = Set.Make(VarVarInt32Comparator)

let dump_vbvset =
  VarBinopVarSet.iter (fun (vl,b,vr) -> 
    f_printf "%s %s %s, " 
     (PpIR.ppVar vl)
     (PpIR.ppBinop b)
     (PpIR.ppVar vr))

let dump_vbvmap =
  IRUtil.VarMap.iter (fun v s_vbv -> 
    f_printf "%s -> {" (PpIR.ppVar v);
    dump_vbvset s_vbv;
    f_printf " }\n")

let dump_varset = IRUtil.VarSet.iter (fun v -> f_printf "%s, " (PpIR.ppVar v)) 

let dump_vsmap =
  IRUtil.VarMap.iter (fun v s_v -> 
    f_printf "%s -> {" (PpIR.ppVar v);
    dump_varset s_v;
    f_printf " }\n")

(* This shit is duplicated from IRLocalOpt, twice! *)
let i64_of_bool = function | true  -> 1L | false -> 0L

(* Given an (IR.var,'a) list, make an IR.var -> 'a map.  What does it do with duplicates? *)
let var_mapify l = List.fold_left (fun a (v,x) -> IRUtil.VarMap.add v x a) IRUtil.VarMap.empty l

let dump_varvarint32set =
  VarVarInt32Set.iter 
   (fun (vsrc,vreg,disp32) -> 
      f_printf "mem[%s.initial+0x%lx] = %s.initial\n" 
       (PpIR.ppVar vreg) disp32 (PpIR.ppVar vsrc)) 
