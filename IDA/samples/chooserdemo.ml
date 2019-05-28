let open IDAGUI in
let blah_chooser = 
{ columns  = [|32,"Int32";64,"String"|];
  describe = (fun (i,s) -> [Printf.sprintf "%08lx" i;s]);
  elements = [|1l,"one";2l,"two";3l,"three"|];
  del      = Some(fun _ i -> IDA.msg "Called del %d\n" i; 0l);
  ins      = Some(fun _   -> IDA.msg "Called ins\n");
  edit     = Some(fun _ i -> IDA.msg "Called edit %d\n" i);
  enter    = Some(fun _ i -> IDA.msg "Called enter %d\n" i);
  destroy  = Some(fun _   -> IDA.msg "Called destroy\n");
} in IDAGUI.choose2 blah_chooser "Test" false;;

