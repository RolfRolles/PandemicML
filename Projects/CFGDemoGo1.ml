#use "c:\\paframework\\framework.ml";;
#use "c:\\paframework\\projects\\cfgdemo.ml";;

let ep = IDA.get_screen_ea () in
let g,h = X86CFG.X86CFGBuilder.build ep in
Hashtbl.iter (fun a _ -> IDA.msg "%08lx: calls %08lx\n" ep a) h;
show_x86_cfg g "Uncollapsed";
show_x86_cfg (X86CFG.X86CFGBuilder.merge_singleton_vertices g) "Collapsed"

