(*
#use "c:\\paframework\\Projects\\Oreans\\CodeVirtualizer\\BreakCV.ml";;
#use "c:\\paframework\\Projects\\Oreans\\CodeVirtualizer\\CVMain.ml";;
#use "c:\\paframework\\Projects\\Oreans\\CodeVirtualizer\\DoCodeGen.ml";;

List.iter do_graph [0x40AAEFl;0x40AE7Bl;0x40B13Al;0x40C0D2l;0x40CCEAl;0x40CE59l;0x40CF1Bl;0x40A80Bl];;
*)

#directory "c:\\paframework\\Projects\\Oreans\\CodeVirtualizer\\";;
#use "CVIRPp.ml";;
#use "CVIRTrans.ml";;
#use "CVCodeGen.ml";;

#directory "c:\\paframework\\Temp\\";;
(*#use "opttarget.ml";;*)


let ea = IDA.get_screen_ea () in
let cfg = get_cluster_as_cfg ea in
let cfg = peephole_cv_graph cfg in
let count_preds v = CVC.G.fold_pred (fun _ i -> i+i) cfg v 0 in
let top_vertices = CVC.G.fold_vertex (fun v l -> if count_preds v = 0 then v::l else l) cfg [] in
let cfg = List.fold_left 
  (fun cfg v -> 
     let ir = CVC.get_ir cfg v in
     List.iter (fun s -> IDA.msg "%s\n" CVDisasm.string_of_cv_instruction s) ir;
     let ir = snd(eat_initial ir)
     CVC.set_ir cfg v) 
   cfg 
   top_vertices 
in
show_cv_cfg cfg "blah";;