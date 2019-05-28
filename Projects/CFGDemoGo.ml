#use "c:\\paframework\\framework.ml";;
#use "c:\\paframework\\projects\\cfgdemo.ml";;

module C = X86CFG.X86CFGBuilder.C;;

let original_graph = fst(X86CFG.X86CFGBuilder.build (IDA.get_screen_ea ()));;
let collapsed_graph = X86CFG.X86CFGBuilder.merge_singleton_vertices original_graph;;

let modify_x86_graph fn cfg = 
  let vertices = C.G.fold_vertex (fun v list -> v::list) collapsed_graph [] in
  List.fold_left 
   (fun cfg v ->
      let ir = C.get_ir cfg v in
      let ir = fn ir in
      C.set_ir cfg v ir)
    collapsed_graph
    vertices;;

let filter_jmp_x86graph =
  let filter_jumps i = 
    let open X86 in 
    match i with
    | { pref = _; instr = (Jmp,_)} -> false 
    | _ -> true
  in modify_x86_graph (List.filter filter_jumps)

let jumps_stripped_graph = filter_jmp_x86graph collapsed_graph;;

show_x86_cfg original_graph "Uncollapsed, unoptimized graph";;
show_x86_cfg collapsed_graph "Collapsed, unoptimized graph";;
show_x86_cfg jumps_stripped_graph "Collapsed, unoptimized, jump-stripped graph";;

let themida_peephole_optimize instrlist =
  let open X86 in
  let rec aux = function
  | [] -> []
  | { pref = _; instr = (Mov,[GeneralReg(Gd(r));Memexpr(Md(Mem32(SS,Some(Esp),None,None)))]) }::
    { pref = _; instr = (Add,[GeneralReg(Gd(Esp));Immediate(Id(4l))]) }::
    xs ->
    aux { pref = []; instr = (Pop,[GeneralReg(Gd(r))]) }::xs
  | x::xs -> x::(aux xs)
  in aux instrlist

let peephole_x86_graph = modify_x86_graph themida_peephole_optimize

      

X86.x86instrpref



