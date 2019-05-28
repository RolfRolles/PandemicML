let mk_var ctx name ty = Z3.mk_const ctx (Z3.mk_string_symbol ctx name) ty
let mk_bool_var ctx name = mk_var ctx name (Z3.mk_bool_sort ctx)
let mk_context ctx = Z3.mk_context_x (Array.append [|("MODEL", "true")|] ctx)

let check ctx expected_result =
  begin
    let (result,m) = Z3.check_and_get_model ctx in
    (match result with
    | Z3.L_FALSE -> IDA.msg "unsat\n";
    | Z3.L_UNDEF ->
      IDA.msg "unknown\n";
      IDA.msg "potential model:\n%s\n" (Z3.model_to_string ctx m);
    | Z3.L_TRUE -> IDA.msg "sat\n%s\n" (Z3.model_to_string ctx m);
    );
    if result != expected_result then IDA.msg "unexpected result";
  end

let find_model_example1() =
  begin
    IDA.msg "\nfind_model_example1\n";
    let ctx     = mk_context [||] in
    let x       = mk_bool_var ctx "x" in
    let y       = mk_bool_var ctx "y" in
    let x_xor_y = Z3.mk_xor ctx x y in
    Z3.assert_cnstr ctx x_xor_y;
    IDA.msg "model for: x xor y\n";
    check ctx Z3.L_TRUE;
    Z3.del_context ctx;
  end

let _ = find_model_example1 ();;