(* http://verify.rwth-aachen.de/seminar/SAT09/propositional-sat.pdf *)
exception BVUnimplemented of string

type bvexpr = 
| BVConstant of int * string
| BVVariable of int * string
| BVAdd of bvexpr * bvexpr
| BVSub of bvexpr * bvexpr
| BVLet of 

let b

let bit_blast_add_z_expr lhs_bit rhs_bit cin_bit = raise(BVUnimplemented("bit_blast_add_z_expr"))

let bit_blast_add_expr lhs rhs =
  let cin_zero = PL.mk_false () in
  let i = num_bits_in_lhs_rhs in
  let rec aux expr cin_bit j = 
    if j >= i
    then cin_bit
    else
     (let lhs_bit   = retrieve_bv_bit_propvar lhs j in
      let rhs_bit   = retrieve_bv_bit_propvar rhs j in
      let z_expr    = bit_blast_add_z_expr    lhs_bit rhs_bit cin_bit in
      let cout_expr = bit_blast_add_cout_expr lhs_bit rhs_bit cin_bit in
      
      ;
      aux (j+1))