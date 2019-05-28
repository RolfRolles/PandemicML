(* File generated from z3.idl *)

type context
and symbol
and ast
and sort = private ast
and func_decl = private ast
and app = private ast
and pattern = private ast
and params
and model
and func_interp
and func_entry
and fixedpoint
and ast_vector
and ast_map
and goal
and tactic
and probe
and apply_result
and solver
and stats



and constructor
and constructor_list

and lbool =
  | L_FALSE
  | L_UNDEF
  | L_TRUE

and symbol_kind =
  | INT_SYMBOL
  | STRING_SYMBOL

and parameter_kind =
  | PARAMETER_INT
  | PARAMETER_DOUBLE
  | PARAMETER_RATIONAL
  | PARAMETER_SYMBOL
  | PARAMETER_SORT
  | PARAMETER_AST
  | PARAMETER_FUNC_DECL

and sort_kind =
  | UNINTERPRETED_SORT
  | BOOL_SORT
  | INT_SORT
  | REAL_SORT
  | BV_SORT
  | ARRAY_SORT
  | DATATYPE_SORT
  | RELATION_SORT
  | FINITE_DOMAIN_SORT
  | UNKNOWN_SORT

and ast_kind =
  | NUMERAL_AST
  | APP_AST
  | VAR_AST
  | QUANTIFIER_AST
  | SORT_AST
  | FUNC_DECL_AST
  | UNKNOWN_AST

and decl_kind =
  | OP_TRUE
  | OP_FALSE
  | OP_EQ
  | OP_DISTINCT
  | OP_ITE
  | OP_AND
  | OP_OR
  | OP_IFF
  | OP_XOR
  | OP_NOT
  | OP_IMPLIES
  | OP_OEQ
  | OP_ANUM
  | OP_AGNUM
  | OP_LE
  | OP_GE
  | OP_LT
  | OP_GT
  | OP_ADD
  | OP_SUB
  | OP_UMINUS
  | OP_MUL
  | OP_DIV
  | OP_IDIV
  | OP_REM
  | OP_MOD
  | OP_TO_REAL
  | OP_TO_INT
  | OP_IS_INT
  | OP_POWER
  | OP_STORE
  | OP_SELECT
  | OP_CONST_ARRAY
  | OP_ARRAY_MAP
  | OP_ARRAY_DEFAULT
  | OP_SET_UNION
  | OP_SET_INTERSECT
  | OP_SET_DIFFERENCE
  | OP_SET_COMPLEMENT
  | OP_SET_SUBSET
  | OP_AS_ARRAY
  | OP_BNUM
  | OP_BIT1
  | OP_BIT0
  | OP_BNEG
  | OP_BADD
  | OP_BSUB
  | OP_BMUL
  | OP_BSDIV
  | OP_BUDIV
  | OP_BSREM
  | OP_BUREM
  | OP_BSMOD
  | OP_BSDIV0
  | OP_BUDIV0
  | OP_BSREM0
  | OP_BUREM0
  | OP_BSMOD0
  | OP_ULEQ
  | OP_SLEQ
  | OP_UGEQ
  | OP_SGEQ
  | OP_ULT
  | OP_SLT
  | OP_UGT
  | OP_SGT
  | OP_BAND
  | OP_BOR
  | OP_BNOT
  | OP_BXOR
  | OP_BNAND
  | OP_BNOR
  | OP_BXNOR
  | OP_CONCAT
  | OP_SIGN_EXT
  | OP_ZERO_EXT
  | OP_EXTRACT
  | OP_REPEAT
  | OP_BREDOR
  | OP_BREDAND
  | OP_BCOMP
  | OP_BSHL
  | OP_BLSHR
  | OP_BASHR
  | OP_ROTATE_LEFT
  | OP_ROTATE_RIGHT
  | OP_EXT_ROTATE_LEFT
  | OP_EXT_ROTATE_RIGHT
  | OP_INT2BV
  | OP_BV2INT
  | OP_CARRY
  | OP_XOR3
  | OP_PR_UNDEF
  | OP_PR_TRUE
  | OP_PR_ASSERTED
  | OP_PR_GOAL
  | OP_PR_MODUS_PONENS
  | OP_PR_REFLEXIVITY
  | OP_PR_SYMMETRY
  | OP_PR_TRANSITIVITY
  | OP_PR_TRANSITIVITY_STAR
  | OP_PR_MONOTONICITY
  | OP_PR_QUANT_INTRO
  | OP_PR_DISTRIBUTIVITY
  | OP_PR_AND_ELIM
  | OP_PR_NOT_OR_ELIM
  | OP_PR_REWRITE
  | OP_PR_REWRITE_STAR
  | OP_PR_PULL_QUANT
  | OP_PR_PULL_QUANT_STAR
  | OP_PR_PUSH_QUANT
  | OP_PR_ELIM_UNUSED_VARS
  | OP_PR_DER
  | OP_PR_QUANT_INST
  | OP_PR_HYPOTHESIS
  | OP_PR_LEMMA
  | OP_PR_UNIT_RESOLUTION
  | OP_PR_IFF_TRUE
  | OP_PR_IFF_FALSE
  | OP_PR_COMMUTATIVITY
  | OP_PR_DEF_AXIOM
  | OP_PR_DEF_INTRO
  | OP_PR_APPLY_DEF
  | OP_PR_IFF_OEQ
  | OP_PR_NNF_POS
  | OP_PR_NNF_NEG
  | OP_PR_NNF_STAR
  | OP_PR_CNF_STAR
  | OP_PR_SKOLEMIZE
  | OP_PR_MODUS_PONENS_OEQ
  | OP_PR_TH_LEMMA
  | OP_RA_STORE
  | OP_RA_EMPTY
  | OP_RA_IS_EMPTY
  | OP_RA_JOIN
  | OP_RA_UNION
  | OP_RA_WIDEN
  | OP_RA_PROJECT
  | OP_RA_FILTER
  | OP_RA_NEGATION_FILTER
  | OP_RA_RENAME
  | OP_RA_COMPLEMENT
  | OP_RA_SELECT
  | OP_RA_CLONE
  | OP_FD_LT
  | OP_LABEL
  | OP_LABEL_LIT
  | OP_UNINTERPRETED

and search_failure =
  | NO_FAILURE
  | UNKNOWN
  | TIMEOUT
  | MEMOUT_WATERMARK
  | CANCELED
  | NUM_CONFLICTS
  | THEORY
  | QUANTIFIERS

and ast_print_mode =
  | PRINT_SMTLIB_FULL
  | PRINT_LOW_LEVEL
  | PRINT_SMTLIB_COMPLIANT
  | PRINT_SMTLIB2_COMPLIANT

and error_code =
  | OK
  | SORT_ERROR
  | IOB
  | INVALID_ARG
  | PARSER_ERROR
  | NO_PARSER
  | INVALID_PATTERN
  | MEMOUT_FAIL
  | FILE_ACCESS_ERROR
  | INTERNAL_FATAL
  | INVALID_USAGE
  | DEC_REF_ERROR
  | EXCEPTION

and goal_prec =
  | GOAL_PRECISE
  | GOAL_UNDER
  | GOAL_OVER
  | GOAL_UNDER_OVER


(**
   

*)
(**
   {2 {L Types}}
   
   
    Most of the types in the API are abstract. 

   
   - [context]: manager of all other Z3 objects, global configuration options, etc.
   - [symbol]: Lisp-like symbol used to name types, constants, and functions.  A symbol can be created using string or integers.
   - [ast]: abstract syntax tree node. That is, the data-structure used in Z3 to represent terms, formulas and types.
   - [sort]: kind of AST used to represent types.
   - [func_decl]: kind of AST used to represent function symbols.
   - [app]: kind of AST used to represent function applications.
   - [pattern]: kind of AST used to represent pattern and multi-patterns used to guide quantifier instantiation.
   
   - [params]: parameter set used to configure many components such as: simplifiers, tactics, solvers, etc.
   - [model]: model for the constraints asserted into the logical context.
   - [func_interp]: interpretation of a function in a model.
   - [func_entry]: representation of the value of a [func_interp] at a particular point.
   - [fixedpoint]: context for the recursive predicate solver.
   - [ast_vector]: vector of [ast] objects.
   - [ast_map]: mapping from [ast] to [ast] objects.
   - [goal]: set of formulas that can be solved and/or transformed using tactics and solvers.
   - [tactic]: basic building block for creating custom solvers for specific problem domains.
   - [probe]: function/predicate used to inspect a goal and collect information that may be used to decide which solver and/or preprocessing step will be used.
   - [apply_result]: collection of subgoals resulting from applying of a tactic to a goal.
   - [solver]: (incremental) solver, possibly specialized by a particular tactic or logic.
   - [stats]: statistical data for a solver.
*)
(**
    {!lbool}  
   Lifted Boolean type: [false], [undefined], [true].
*)
(**
    {!symbol_kind}  
   The different kinds of symbol.
   In Z3, a symbol can be represented using integers and strings (See {!get_symbol_kind}).

   - {b See also}: {!mk_int_symbol}
   - {b See also}: {!mk_string_symbol}
*)
(**
    {!parameter_kind}  
   The different kinds of parameters that can be associated with function symbols.
   - {b See also}: {!get_decl_num_parameters}
   - {b See also}: {!get_decl_parameter_kind}

   - PARAMETER_INT is used for integer parameters.
   - PARAMETER_DOUBLE is used for double parameters.
   - PARAMETER_RATIONAL is used for parameters that are rational numbers.
   - PARAMETER_SYMBOL is used for parameters that are symbols.
   - PARAMETER_SORT is used for sort parameters.
   - PARAMETER_AST is used for expression parameters.
   - PARAMETER_FUNC_DECL is used for function declaration parameters.
*)
(**
    {!sort_kind}  
   The different kinds of Z3 types (See {!get_sort_kind}).
*)
(**
    {!ast_kind}  
   The different kinds of Z3 AST (abstract syntax trees). That is, terms, formulas and types.

   - APP_AST:            constant and applications
   - NUMERAL_AST:        numeral constants
   - VAR_AST:            bound variables
   - QUANTIFIER_AST:     quantifiers
   - SORT_AST:           sort
   - FUNC_DECL_AST:      function declaration
   - UNKNOWN_AST:        internal
*)
(**
    {!decl_kind}  
   The different kinds of interpreted function kinds.

   - OP_TRUE The constant true.

   - OP_FALSE The constant false.

   - OP_EQ The equality predicate.

   - OP_DISTINCT The n-ary distinct predicate (every argument is mutually distinct).

   - OP_ITE The ternary if-then-else term.

   - OP_AND n-ary conjunction.

   - OP_OR n-ary disjunction.

   - OP_IFF equivalence (binary).

   - OP_XOR Exclusive or.

   - OP_NOT Negation.

   - OP_IMPLIES Implication.

   - OP_OEQ Binary equivalence modulo namings. This binary predicate is used in proof terms.
        It captures equisatisfiability and equivalence modulo renamings.

   - OP_ANUM Arithmetic numeral.

   - OP_AGNUM Arithmetic algebraic numeral. Algebraic numbers are used to represent irrational numbers in Z3.

   - OP_LE <=.

   - OP_GE >=.

   - OP_LT <.

   - OP_GT >.

   - OP_ADD Addition - Binary.

   - OP_SUB Binary subtraction.

   - OP_UMINUS Unary minus.

   - OP_MUL Multiplication - Binary.

   - OP_DIV Division - Binary.

   - OP_IDIV Integer division - Binary.

   - OP_REM Remainder - Binary.

   - OP_MOD Modulus - Binary.

   - OP_TO_REAL Coercion of integer to real - Unary.

   - OP_TO_INT Coercion of real to integer - Unary.

   - OP_IS_INT Check if real is also an integer - Unary.

   - OP_POWER Power operator x^y.

   - OP_STORE Array store. It satisfies select(store(a,i,v),j) = if i = j then v else select(a,j).
        Array store takes at least 3 arguments. 

   - OP_SELECT Array select. 

   - OP_CONST_ARRAY The constant array. For example, select(const(v),i) = v holds for every v and i. The function is unary.

   - OP_ARRAY_DEFAULT Default value of arrays. For example default(const(v)) = v. The function is unary.

   - OP_ARRAY_MAP Array map operator.
         It satisfies map[f](a1,..,a_n)[i] = f(a1[i],...,a_n[i]) for every i.

   - OP_SET_UNION Set union between two Booelan arrays (two arrays whose range type is Boolean). The function is binary.

   - OP_SET_INTERSECT Set intersection between two Boolean arrays. The function is binary.

   - OP_SET_DIFFERENCE Set difference between two Boolean arrays. The function is binary.

   - OP_SET_COMPLEMENT Set complement of a Boolean array. The function is unary.

   - OP_SET_SUBSET Subset predicate between two Boolean arrays. The relation is binary.

   - OP_AS_ARRAY An array value that behaves as the function graph of the
                    function passed as parameter.

   - OP_BNUM Bit-vector numeral.

   - OP_BIT1 One bit bit-vector.

   - OP_BIT0 Zero bit bit-vector.

   - OP_BNEG Unary minus.

   - OP_BADD Binary addition.

   - OP_BSUB Binary subtraction.

   - OP_BMUL Binary multiplication.
    
   - OP_BSDIV Binary signed division.

   - OP_BUDIV Binary unsigned int division.

   - OP_BSREM Binary signed remainder.

   - OP_BUREM Binary unsigned int remainder.

   - OP_BSMOD Binary signed modulus.

   - OP_BSDIV0 Unary function. bsdiv(x,0) is congruent to bsdiv0(x).

   - OP_BUDIV0 Unary function. budiv(x,0) is congruent to budiv0(x).

   - OP_BSREM0 Unary function. bsrem(x,0) is congruent to bsrem0(x).

   - OP_BUREM0 Unary function. burem(x,0) is congruent to burem0(x).

   - OP_BSMOD0 Unary function. bsmod(x,0) is congruent to bsmod0(x).
    
   - OP_ULEQ Unsigned bit-vector <= - Binary relation.

   - OP_SLEQ Signed bit-vector  <= - Binary relation.

   - OP_UGEQ Unsigned bit-vector  >= - Binary relation.

   - OP_SGEQ Signed bit-vector  >= - Binary relation.

   - OP_ULT Unsigned bit-vector  < - Binary relation.

   - OP_SLT Signed bit-vector < - Binary relation.

   - OP_UGT Unsigned bit-vector > - Binary relation.

   - OP_SGT Signed bit-vector > - Binary relation.

   - OP_BAND Bit-wise and - Binary.

   - OP_BOR Bit-wise or - Binary.

   - OP_BNOT Bit-wise not - Unary.

   - OP_BXOR Bit-wise xor - Binary.

   - OP_BNAND Bit-wise nand - Binary.

   - OP_BNOR Bit-wise nor - Binary.

   - OP_BXNOR Bit-wise xnor - Binary.

   - OP_CONCAT Bit-vector concatenation - Binary.

   - OP_SIGN_EXT Bit-vector sign extension.

   - OP_ZERO_EXT Bit-vector zero extension.

   - OP_EXTRACT Bit-vector extraction.

   - OP_REPEAT Repeat bit-vector n times.

   - OP_BREDOR Bit-vector reduce or - Unary.

   - OP_BREDAND Bit-vector reduce and - Unary.

   - OP_BCOMP .

   - OP_BSHL Shift left.

   - OP_BLSHR Logical shift right.

   - OP_BASHR Arithmetical shift right.

   - OP_ROTATE_LEFT Left rotation.

   - OP_ROTATE_RIGHT Right rotation.

   - OP_EXT_ROTATE_LEFT (extended) Left rotation. Similar to OP_ROTATE_LEFT, but it is a binary operator instead of a parametric one.

   - OP_EXT_ROTATE_RIGHT (extended) Right rotation. Similar to OP_ROTATE_RIGHT, but it is a binary operator instead of a parametric one.

   - OP_INT2BV Coerce integer to bit-vector. NB. This function
       is not supported by the decision procedures. Only the most
       rudimentary simplification rules are applied to this function.

   - OP_BV2INT Coerce bit-vector to integer. NB. This function
       is not supported by the decision procedures. Only the most
       rudimentary simplification rules are applied to this function.

   - OP_CARRY Compute the carry bit in a full-adder. 
       The meaning is given by the equivalence
       (carry l1 l2 l3) <=> (or (and l1 l2) (and l1 l3) (and l2 l3)))

   - OP_XOR3 Compute ternary XOR.
       The meaning is given by the equivalence
       (xor3 l1 l2 l3) <=> (xor (xor l1 l2) l3)

   - OP_PR_UNDEF: Undef/Null proof object.

   - OP_PR_TRUE: Proof for the expression 'true'.

   - OP_PR_ASSERTED: Proof for a fact asserted by the user.
   
   - OP_PR_GOAL: Proof for a fact (tagged as goal) asserted by the user.

   - OP_PR_MODUS_PONENS: Given a proof for p and a proof for (implies p q), produces a proof for q.
       {e
          T1: p
          T2: (implies p q)
          [mp T1 T2]: q
          }
          The second antecedents may also be a proof for (iff p q).

   - OP_PR_REFLEXIVITY: A proof for (R t t), where R is a reflexive relation. This proof object has no antecedents.
        The only reflexive relations that are used are 
        equivalence modulo namings, equality and equivalence.
        That is, R is either '~', '=' or 'iff'.

   - OP_PR_SYMMETRY: Given an symmetric relation R and a proof for (R t s), produces a proof for (R s t).
          {e
          T1: (R t s)
          [symmetry T1]: (R s t)
          }
          T1 is the antecedent of this proof object.

   - OP_PR_TRANSITIVITY: Given a transitive relation R, and proofs for (R t s) and (R s u), produces a proof
       for (R t u).
       {e
       T1: (R t s)
       T2: (R s u)
       [trans T1 T2]: (R t u)
       }

   - OP_PR_TRANSITIVITY_STAR: Condensed transitivity proof. This proof object is only used if the parameter PROOF_MODE is 1.
     It combines several symmetry and transitivity proofs. 

          Example:
          {e
          T1: (R a b)
          T2: (R c b)
          T3: (R c d)
          [trans* T1 T2 T3]: (R a d)
          }
          R must be a symmetric and transitive relation.

          Assuming that this proof object is a proof for (R s t), then
          a proof checker must check if it is possible to prove (R s t)
          using the antecedents, symmetry and transitivity.  That is, 
          if there is a path from s to t, if we view every
          antecedent (R a b) as an edge between a and b.

   - OP_PR_MONOTONICITY: Monotonicity proof object.
          {e
          T1: (R t_1 s_1)
          ...
          Tn: (R t_n s_n)
          [monotonicity T1 ... Tn]: (R (f t_1 ... t_n) (f s_1 ... s_n))
          }
          Remark: if t_i == s_i, then the antecedent Ti is suppressed.
          That is, reflexivity proofs are supressed to save space.

   - OP_PR_QUANT_INTRO: Given a proof for (~ p q), produces a proof for (~ (forall (x) p) (forall (x) q)).

       T1: (~ p q)
       [quant-intro T1]: (~ (forall (x) p) (forall (x) q))
   
   - OP_PR_DISTRIBUTIVITY: Distributivity proof object. 
          Given that f (= or) distributes over g (= and), produces a proof for

          (= (f a (g c d))
             (g (f a c) (f a d)))

          If f and g are associative, this proof also justifies the following equality:

          (= (f (g a b) (g c d))
             (g (f a c) (f a d) (f b c) (f b d)))

          where each f and g can have arbitrary number of arguments.

          This proof object has no antecedents.
          Remark. This rule is used by the CNF conversion pass and 
          instantiated by f = or, and g = and.
    
   - OP_PR_AND_ELIM: Given a proof for (and l_1 ... l_n), produces a proof for l_i
        
       {e
       T1: (and l_1 ... l_n)
       [and-elim T1]: l_i
       }
   - OP_PR_NOT_OR_ELIM: Given a proof for (not (or l_1 ... l_n)), produces a proof for (not l_i).

       {e
       T1: (not (or l_1 ... l_n))
       [not-or-elim T1]: (not l_i)
       }

   - OP_PR_REWRITE: A proof for a local rewriting step (= t s).
          The head function symbol of t is interpreted.

          This proof object has no antecedents.
          The conclusion of a rewrite rule is either an equality (= t s), 
          an equivalence (iff t s), or equi-satisfiability (~ t s).
          Remark: if f is bool, then = is iff.
          

          Examples:
          {e
          (= (+ x 0) x)
          (= (+ x 1 2) (+ 3 x))
          (iff (or x false) x)
          }

   - OP_PR_REWRITE_STAR: A proof for rewriting an expression t into an expression s.
       This proof object is used if the parameter PROOF_MODE is 1.
       This proof object can have n antecedents.
       The antecedents are proofs for equalities used as substitution rules.
       The object is also used in a few cases if the parameter PROOF_MODE is 2.
       The cases are:
         - When applying contextual simplification (CONTEXT_SIMPLIFIER=true)
         - When converting bit-vectors to Booleans (BIT2BOOL=true)
         - When pulling ite expression up (PULL_CHEAP_ITE_TREES=true)

   - OP_PR_PULL_QUANT: A proof for (iff (f (forall (x) q(x)) r) (forall (x) (f (q x) r))). This proof object has no antecedents.

   - OP_PR_PULL_QUANT_STAR: A proof for (iff P Q) where Q is in prenex normal form.
       This proof object is only used if the parameter PROOF_MODE is 1.       
       This proof object has no antecedents.
  
   - OP_PR_PUSH_QUANT: A proof for:

       {e
          (iff (forall (x_1 ... x_m) (and p_1[x_1 ... x_m] ... p_n[x_1 ... x_m]))
               (and (forall (x_1 ... x_m) p_1[x_1 ... x_m])
                 ... 
               (forall (x_1 ... x_m) p_n[x_1 ... x_m])))
               }
         This proof object has no antecedents.

   - OP_PR_ELIM_UNUSED_VARS:  
          A proof for (iff (forall (x_1 ... x_n y_1 ... y_m) p[x_1 ... x_n])
                           (forall (x_1 ... x_n) p[x_1 ... x_n])) 

          It is used to justify the elimination of unused variables.
          This proof object has no antecedents.

   - OP_PR_DER: A proof for destructive equality resolution:
          (iff (forall (x) (or (not (= x t)) P[x])) P[t])
          if x does not occur in t.

          This proof object has no antecedents.
          
          Several variables can be eliminated simultaneously.

   - OP_PR_QUANT_INST: A proof of (or (not (forall (x) (P x))) (P a))

   - OP_PR_HYPOTHESIS: Mark a hypothesis in a natural deduction style proof.

   - OP_PR_LEMMA: 

       {e
          T1: false
          [lemma T1]: (or (not l_1) ... (not l_n))
          }
          This proof object has one antecedent: a hypothetical proof for false.
          It converts the proof in a proof for (or (not l_1) ... (not l_n)),
          when T1 contains the hypotheses: l_1, ..., l_n.

   - OP_PR_UNIT_RESOLUTION: 
       {e
          T1:      (or l_1 ... l_n l_1' ... l_m')
          T2:      (not l_1)
          ...
          T(n+1):  (not l_n)
          [unit-resolution T1 ... T(n+1)]: (or l_1' ... l_m')
          }

   - OP_PR_IFF_TRUE: 
      {e
       T1: p
       [iff-true T1]: (iff p true)
       }

   - OP_PR_IFF_FALSE:
      {e
       T1: (not p)
       [iff-false T1]: (iff p false)
       }

   - OP_PR_COMMUTATIVITY:

          [comm]: (= (f a b) (f b a))
          
          f is a commutative operator.

          This proof object has no antecedents.
          Remark: if f is bool, then = is iff.
   
   - OP_PR_DEF_AXIOM: Proof object used to justify Tseitin's like axioms:
       
          {e
          (or (not (and p q)) p)
          (or (not (and p q)) q)
          (or (not (and p q r)) p)
          (or (not (and p q r)) q)
          (or (not (and p q r)) r)
          ...
          (or (and p q) (not p) (not q))
          (or (not (or p q)) p q)
          (or (or p q) (not p))
          (or (or p q) (not q))
          (or (not (iff p q)) (not p) q)
          (or (not (iff p q)) p (not q))
          (or (iff p q) (not p) (not q))
          (or (iff p q) p q)
          (or (not (ite a b c)) (not a) b)
          (or (not (ite a b c)) a c)
          (or (ite a b c) (not a) (not b))
          (or (ite a b c) a (not c))
          (or (not (not a)) (not a))
          (or (not a) a)
          }
          This proof object has no antecedents.
          Note: all axioms are propositional tautologies.
          Note also that 'and' and 'or' can take multiple arguments.
          You can recover the propositional tautologies by
          unfolding the Boolean connectives in the axioms a small
          bounded number of steps (=3).
    
   - OP_PR_DEF_INTRO: Introduces a name for a formula/term.
       Suppose e is an expression with free variables x, and def-intro
       introduces the name n(x). The possible cases are:

       When e is of Boolean type:
       [def-intro]: (and (or n (not e)) (or (not n) e))

       or:
       [def-intro]: (or (not n) e)
       when e only occurs positively.

       When e is of the form (ite cond th el):
       [def-intro]: (and (or (not cond) (= n th)) (or cond (= n el)))

       Otherwise:
       [def-intro]: (= n e)       

   - OP_PR_APPLY_DEF: 
       [apply-def T1]: F ~ n
       F is 'equivalent' to n, given that T1 is a proof that
       n is a name for F.
   
   - OP_PR_IFF_OEQ:
       T1: (iff p q)
       [iff~ T1]: (~ p q)
 
   - OP_PR_NNF_POS: Proof for a (positive) NNF step. Example:
       {e
          T1: (not s_1) ~ r_1
          T2: (not s_2) ~ r_2
          T3: s_1 ~ r_1'
          T4: s_2 ~ r_2'
          [nnf-pos T1 T2 T3 T4]: (~ (iff s_1 s_2)
                                    (and (or r_1 r_2') (or r_1' r_2)))
          }
       The negation normal form steps NNF_POS and NNF_NEG are used in the following cases:
       (a) When creating the NNF of a positive force quantifier.
        The quantifier is retained (unless the bound variables are eliminated).
        Example
        {e
           T1: q ~ q_new 
           [nnf-pos T1]: (~ (forall (x T) q) (forall (x T) q_new))
        }
       (b) When recursively creating NNF over Boolean formulas, where the top-level
       connective is changed during NNF conversion. The relevant Boolean connectives
       for NNF_POS are 'implies', 'iff', 'xor', 'ite'.
       NNF_NEG furthermore handles the case where negation is pushed
       over Boolean connectives 'and' and 'or'.

    
   - OP_PR_NFF_NEG: Proof for a (negative) NNF step. Examples:
          {e
          T1: (not s_1) ~ r_1
          ...
          Tn: (not s_n) ~ r_n
         [nnf-neg T1 ... Tn]: (not (and s_1 ... s_n)) ~ (or r_1 ... r_n)
      and
          T1: (not s_1) ~ r_1
          ...
          Tn: (not s_n) ~ r_n
         [nnf-neg T1 ... Tn]: (not (or s_1 ... s_n)) ~ (and r_1 ... r_n)
      and
          T1: (not s_1) ~ r_1
          T2: (not s_2) ~ r_2
          T3: s_1 ~ r_1'
          T4: s_2 ~ r_2'
         [nnf-neg T1 T2 T3 T4]: (~ (not (iff s_1 s_2))
                                   (and (or r_1 r_2) (or r_1' r_2')))
       }
   - OP_PR_NNF_STAR: A proof for (~ P Q) where Q is in negation normal form.
       
       This proof object is only used if the parameter PROOF_MODE is 1.       
              
       This proof object may have n antecedents. Each antecedent is a PR_DEF_INTRO.

   - OP_PR_CNF_STAR: A proof for (~ P Q) where Q is in conjunctive normal form.
       This proof object is only used if the parameter PROOF_MODE is 1.       
       This proof object may have n antecedents. Each antecedent is a PR_DEF_INTRO.          

   - OP_PR_SKOLEMIZE: Proof for:  
       
          {e
          [sk]: (~ (not (forall x (p x y))) (not (p (sk y) y)))
          [sk]: (~ (exists x (p x y)) (p (sk y) y))
          }

          This proof object has no antecedents.
   
   - OP_PR_MODUS_PONENS_OEQ: Modus ponens style rule for equi-satisfiability.
       {e
          T1: p
          T2: (~ p q)
          [mp~ T1 T2]: q
          }

    - OP_PR_TH_LEMMA: Generic proof for theory lemmas.

         The theory lemma function comes with one or more parameters.
         The first parameter indicates the name of the theory.
         For the theory of arithmetic, additional parameters provide hints for
         checking the theory lemma. 
         The hints for arithmetic are:
         
         - farkas - followed by rational coefficients. Multiply the coefficients to the
           inequalities in the lemma, add the (negated) inequalities and obtain a contradiction.

         - triangle-eq - Indicates a lemma related to the equivalence:
         {e
            (iff (= t1 t2) (and (<= t1 t2) (<= t2 t1)))
         }

         - gcd-test - Indicates an integer linear arithmetic lemma that uses a gcd test.


      - OP_RA_STORE: Insert a record into a relation.
        The function takes [n+1] arguments, where the first argument is the relation and the remaining [n] elements 
        correspond to the [n] columns of the relation.

      - OP_RA_EMPTY: Creates the empty relation. 
        
      - OP_RA_IS_EMPTY: Tests if the relation is empty.

      - OP_RA_JOIN: Create the relational join.

      - OP_RA_UNION: Create the union or convex hull of two relations. 
        The function takes two arguments.

      - OP_RA_WIDEN: Widen two relations.
        The function takes two arguments.

      - OP_RA_PROJECT: Project the columns (provided as numbers in the parameters).
        The function takes one argument.

      - OP_RA_FILTER: Filter (restrict) a relation with respect to a predicate.
        The first argument is a relation. 
        The second argument is a predicate with free de-Brujin indices
        corresponding to the columns of the relation.
        So the first column in the relation has index 0.

      - OP_RA_NEGATION_FILTER: Intersect the first relation with respect to negation
        of the second relation (the function takes two arguments).
        Logically, the specification can be described by a function

           target = filter_by_negation(pos, neg, columns)

        where columns are pairs c1, d1, .., cN, dN of columns from pos and neg, such that
        target are elements in x in pos, such that there is no y in neg that agrees with
        x on the columns c1, d1, .., cN, dN.

    
      - OP_RA_RENAME: rename columns in the relation. 
        The function takes one argument.
        The parameters contain the renaming as a cycle.
         
      - OP_RA_COMPLEMENT: Complement the relation.

      - OP_RA_SELECT: Check if a record is an element of the relation.
        The function takes [n+1] arguments, where the first argument is a relation,
        and the remaining [n] arguments correspond to a record.

      - OP_RA_CLONE: Create a fresh copy (clone) of a relation. 
        The function is logically the identity, but
        in the context of a register machine allows
        for  [OP_RA_UNION]  
        to perform destructive updates to the first argument.
        

      - OP_FD_LT: A less than predicate over the finite domain FINITE_DOMAIN_SORT.

      - OP_LABEL: A label (used by the Boogie Verification condition generator).
                     The label has two parameters, a string and a Boolean polarity.
                     It takes one argument, a formula.

      - OP_LABEL_LIT: A label literal (used by the Boogie Verification condition generator).
                     A label literal has a set of string parameters. It takes no arguments.

      - OP_UNINTERPRETED: kind used for uninterpreted symbols.

*)
(**
    {!search_failure}  
   The different kinds of search failure types.

   - NO_FAILURE:         The last search was successful
   - UNKNOWN:            Undocumented failure reason
   - TIMEOUT:            Timeout
   - MEMOUT_WATERMAK:    Search hit a memory high-watermak limit
   - CANCELED:           val cancel flag was set
   - NUM_CONFLICTS:      Maximum number of conflicts was reached
   - THEORY:             Theory is incomplete
   - QUANTIFIERS:        Logical context contains universal quantifiers
*)
(**
    {!ast_print_mode}  
   Z3 pretty printing modes (See {!set_ast_print_mode}).

   - PRINT_SMTLIB_FULL:   Print AST nodes in SMTLIB verbose format.
   - PRINT_LOW_LEVEL:     Print AST nodes using a low-level format.
   - PRINT_SMTLIB_COMPLIANT: Print AST nodes in SMTLIB 1.x compliant format.
   - PRINT_SMTLIB2_COMPLIANT: Print AST nodes in SMTLIB 2.x compliant format.
*)
(**
    {!error_code}  
   Z3 error codes 
   
   - OK:            No error.
   - SORT_ERROR:    User tried to build an invalid (type incorrect) AST.
   - IOB:           Index out of bounds.
   - INVALID_ARG:   Invalid argument was provided.
   - PARSER_ERROR:  An error occurred when parsing a string or file.
   - NO_PARSER:     Parser output is not available, that is, user didn't invoke {!parse_smtlib_string} or {!parse_smtlib_file}.
   - INVALID_PATTERN: Invalid pattern was used to build a quantifier.
   - MEMOUT_FAIL:   A memory allocation failure was encountered.
   - FILE_ACCESS_ERRROR: A file could not be accessed.
   - INVALID_USAGE:   API call is invalid in the current state.
   - INTERNAL_FATAL: An error internal to Z3 occurred.
   - DEC_REF_ERROR: Trying to decrement the reference counter of an AST that was deleted or the reference counter was not initialized.
   - EXCEPTION:     Internal Z3 exception. Additional details can be retrieved using  {!get_error_msg}.  
*)

(** Exceptions raised by Z3. It is safe to continue interacting with Z3 after
    catching [Error] exceptions.

    - {b See also}: {!get_error_msg}
*)
exception Error of context * error_code

(**
    {!goal_prec}  
   A Goal is essentially a set of formulas. Z3 provide APIs for building strategies/tactics for solving and transforming Goals. Some of these transformations apply under/over approximations.
   
   - GOAL_PRECISE:    Approximations/Relaxations were not applied on the goal (sat and unsat answers were preserved).
   - GOAL_UNDER:      Goal is the product of a under-approximation (sat answers are preserved).
   - GOAL_OVER:       Goal is the product of an over-approximation (unsat answers are preserved).
   - GOAL_UNDER_OVER: Goal is garbage (it is the product of over- and under-approximations, sat and unsat answers are not preserved).
*)
(**
       {2 {L Create context}}
*)
(**
       Summary: Create a context using the given configuration. 
    
       After a context is created, the configuration cannot be changed,
       although some parameters can be changed using {!update_param_value}.
       All main interaction with Z3 happens in the context of a [context].

       

       
*)
val mk_context: (string * string) list -> context 

(**
       Summary: Update a mutable configuration parameter.

       The list of all configuration parameters can be obtained using the Z3 executable:

       {v 
       z3.exe -ini?
        v}

       Only a few configuration parameters are mutable once the context is created.
       The error handler is invoked when trying to modify an immutable parameter.

       
        - {b See also}: {!mk_context }
*)
val update_param_value : context -> string -> string -> unit
	

(**
       Summary: Get a configuration parameter.
      
       Returns  [None]  
       if the parameter value does not exist.

       
       
        - {b See also}: {!mk_context }
*)
val get_param_value : context -> string -> string option
	

(**
       Summary: Interrupt the execution of a Z3 procedure.
       This procedure can be used to interrupt: solvers, simplifiers and tactics.
*)
val interrupt : context -> unit
	

(**
       {2 {L Parameters}}
*)
(**
       Summary: Create a Z3 (empty) parameter set.
       Starting at Z3 4.0, parameter sets are used to configure many components such as:
       simplifiers, tactics, solvers, etc.

       
       
*)
val mk_params : context -> params
	

(**
       Summary: Add a Boolean parameter [k] with value [v] to the parameter set [p].
*)
val params_set_bool : context -> params -> symbol -> bool -> unit
	

(**
       Summary: Add a unsigned int parameter [k] with value [v] to the parameter set [p].
*)
val params_set_uint : context -> params -> symbol -> int -> unit
	

(**
       Summary: Add a double parameter [k] with value [v] to the parameter set [p].
*)
val params_set_double : context -> params -> symbol -> float -> unit
	

(**
       Summary: Add a symbol parameter [k] with value [v] to the parameter set [p].
*)
val params_set_symbol : context -> params -> symbol -> symbol -> unit
	

(**
       Summary: Convert a parameter set into a string. This function is mainly used for printing the
       contents of a parameter set.
*)
val params_to_string : context -> params -> string
	

(**
       {2 {L Symbols}}
*)

(**
   Refined view of a {!symbol}.

   - {b See also}: {!mk_symbol}
   - {b See also}: {!symbol_refine}
*)
type symbol_refined =
  | Symbol_int of int
  | Symbol_string of string


(**
  Summary: \[ [ mk_symbol c sr ] \] constructs the symbol described by [sr].

  - {b See also}: {!symbol_refine}
*)
val mk_symbol: context -> symbol_refined -> symbol

(**
        {4 {L Redundant low-level API}} 
*)
(**
       Summary: Create a Z3 symbol using an integer.

       Symbols are used to name several term and type constructors.

       NB. Not all integers can be passed to this function.
       The legal range of unsigned int integers is 0 to 2^30-1.

       - {b See also}: {!mk_string_symbol}
*)
val mk_int_symbol : context -> int -> symbol
	

(**
       Summary: Create a Z3 symbol using a C string.

       Symbols are used to name several term and type constructors.

       - {b See also}: {!mk_int_symbol}
*)
val mk_string_symbol : context -> string -> symbol
	

(**
       {2 {L Sorts}}
*)

(**
   A datatype constructor descriptor.
*)
type datatype_constructor_desc = {
  constructor_desc : symbol;            	(** name of the constructor function *)
  recognizer_desc : symbol;             	(** name of the recognizer function *)
  accessor_descs : (symbol * sort) array;	(** names and sorts of the fields *)
}

(**
   A datatype is described by a name and constructor descriptors.
*)
type datatype_desc = symbol * datatype_constructor_desc array

(**
   A datatype constructor representation.
*)
type datatype_constructor = {
  constructor : func_decl;              	(** constructor function *)
  recognizer : func_decl;               	(** recognizer function *)
  accessors : func_decl array;          	(** field accessor functions *)
}

(**
   A datatype is represented by a sort and constructors.
*)
type datatype = sort * datatype_constructor array

(**
   Refined view of a {!sort}.

   - {b See also}: {!mk_sort}
   - {b See also}: {!sort_refine}
*)
type sort_refined =
  | Sort_uninterpreted of symbol
  | Sort_bool
  | Sort_int
  | Sort_bv of int
  | Sort_finite_domain of symbol * int64
  | Sort_real
  | Sort_array of sort * sort
  | Sort_datatype of datatype_constructor array
  | Sort_relation of sort array
  | Sort_unknown


(**
   Summary: \[ [ mk_sort c sr ] \] constructs the sort described by [sr].

   - {b Precondition}: [sr] is not of form [Sort_relation] or [Sort_unknown], which cannot be directly constructed
   - {b See also}: {!mk_datatypes}
   - {b See also}: {!sort_refine}
*)
val mk_sort: context -> sort_refined -> sort

(**
   \[ [mk_datatypes ctx sorts_to_descriptors] \] creates mutually recursive datatypes described by
   [sorts_to_descriptors], which is a function from the sorts of the datatypes to be created to
   descriptors of the datatypes' constructors.

   - {b See also}: {!Test_mlapi.forest_example}
*)
val mk_datatypes: context -> (sort array -> (datatype_desc array) option) -> datatype array

(**
        {4 {L Redundant low-level API}} 
*)
(**
       Summary: Create a free (uninterpreted) type using the given name (symbol).
       
       Two free types are considered the same iff the have the same name.
*)
val mk_uninterpreted_sort : context -> symbol -> sort
	

(**
       Summary: Create the Boolean type. 

       This type is used to create propositional variables and predicates.
*)
val mk_bool_sort : context -> sort
	

(**
       Summary: Create the integer type.

       This type is not the int type found in programming languages.
       A machine integer can be represented using bit-vectors. The function
       {!mk_bv_sort} creates a bit-vector type.

       - {b See also}: {!mk_bv_sort}
*)
val mk_int_sort : context -> sort
	

(**
       Summary: Create the real type. 

       This type is not a floating point number.
       Z3 does not have support for floating point numbers yet.
*)
val mk_real_sort : context -> sort
	

(**
       Summary: Create a bit-vector type of the given size.
    
       This type can also be seen as a machine integer.

       - {b Remarks}: The size of the bitvector type must be greater than zero.
*)
val mk_bv_sort : context -> int -> sort
	

(**
       Summary: Create a named finite domain sort.

       To create constants that belong to the finite domain, 
       use the APIs for creating numerals and pass a numeric
       constant together with the sort returned by this call.

       - {b See also}: {!get_finite_domain_sort_size.}
*)
val mk_finite_domain_sort : context -> symbol -> int64 -> sort
	

(**
       Summary: Create an array type. 
       
       We usually represent the array type as: {e [domain -> range] }.
       Arrays are usually used to model the heap/memory in software verification.

       - {b See also}: {!mk_select}
       - {b See also}: {!mk_store}
*)
val mk_array_sort : context -> sort -> sort -> sort
	

(**
       Summary: Create a tuple type.
       
        [mk_tuple_sort c name field_names field_sorts] creates a tuple with a constructor named [name],
       a [n] fields, where [n] is the size of the arrays [field_names] and [field_sorts].
       

       
       

       @param c logical context
       @param mk_tuple_name name of the constructor function associated with the tuple type.
       @param num_fields number of fields in the tuple type.
       @param field_names name of the projection functions.
       @param field_sorts type of the tuple fields.
       @param mk_tuple_decl output parameter that will contain the constructor declaration.
       @param proj_decl output parameter that will contain the projection function declarations. This field must be a buffer of size [num_fields] allocated by the user.
*)
val mk_tuple_sort : context -> symbol -> symbol array -> sort array -> sort * func_decl * func_decl array
	

(**
       Summary: Create a enumeration sort.
       
        [mk_enumeration_sort c enums] creates an enumeration sort with enumeration names [enums], 
               it also returns [n] predicates, where [n] is the number of [enums] corresponding
               to testing whether an element is one of the enumerants.
       

       
       
       
       @param c logical context
       @param name name of the enumeration sort.
       @param n number of elemenets in enumeration sort.
       @param enum_names names of the enumerated elements.
       @param enum_consts constants corresponding to the enumerated elements.
       @param enum_testers predicates testing if terms of the enumeration sort correspond to an enumeration.

       For example, if this function is called with three symbols A, B, C and the name S, then 
       [s] is a sort whose name is S, and the function returns three terms corresponding to A, B, C in 
       [enum_consts]. The array [enum_testers] has three predicates of type {e (s -> Bool) }.
       The first predicate (corresponding to A) is true when applied to A, and false otherwise.
       Similarly for the other predicates.
*)
val mk_enumeration_sort : context -> symbol -> symbol array -> sort * func_decl array * func_decl array
	

(**
       Summary: Create a list sort
       
        [mk_list_sort c name elem_sort] creates a list sort of [name], over elements of sort [elem_sort].
       

       
       

       @param c logical context
       @param name name of the list sort.
       @param elem_sort sort of list elements.
       @param nil_decl declaration for the empty list.
       @param is_nil_decl test for the empty list.
       @param cons_decl declaration for a cons cell.
       @param is_cons_decl cons cell test.
       @param head_decl list head.
       @param tail_decl list tail.
*)
val mk_list_sort : context -> symbol -> sort -> sort * func_decl * func_decl * func_decl * func_decl * func_decl * func_decl
	

(*
(**
       Summary: Create a constructor.
       
       @param c logical context.
       @param name constructor name.
       @param recognizer name of recognizer function.
       @param num_fields number of fields in constructor.
       @param field_names names of the constructor fields.
       @param sorts field sorts,  [None]  
                    if the field sort refers to a recursive sort.
       @param sort_refs reference to datatype sort that is an argument to the constructor; if the corresponding
                        sort reference is  [None],  
                        then the value in sort_refs should be an index referring to 
                        one of the recursive datatypes that is declared.                        
*)
val mk_constructor : context -> symbol -> symbol -> symbol array -> sort option array -> int array -> constructor
	

(**
       Summary: Reclaim memory allocated to constructor.

       @param c logical context.
       @param constr constructor.
*)
val del_constructor : context -> constructor -> unit
	

(**
       Summary: Create datatype, such as lists, trees, records, enumerations or unions of records. 
       The datatype may be recursive. Return the datatype sort.

       @param c logical context.
	   @param name name of datatype.
       @param num_constructors number of constructors passed in.
       @param constructors array of constructor containers.
*)
val mk_datatype : context -> symbol -> constructor array -> sort * constructor array
	

(**
       Summary: Create list of constructors.

       @param c logical context.
       @param num_constructors number of constructors in list.
       @param constructors list of constructors.
*)
val mk_constructor_list : context -> constructor array -> constructor_list
	

(**
       Summary: Reclaim memory allocated for constructor list.

       Each constructor inside the constructor list must be independently reclaimed using {!del_constructor}.

       @param c logical context.
       @param clist constructor list container.

*)
val del_constructor_list : context -> constructor_list -> unit
	

(**
       Summary: Create mutually recursive datatypes.

       @param c logical context.
       @param num_sorts number of datatype sorts.
       @param sort_names names of datatype sorts.
       @param sorts array of datattype sorts.
       @param constructor_lists list of constructors, one list per sort.
*)
val mk_datatypes : context -> symbol array -> constructor_list array -> sort array * constructor_list array
	

(**
       Summary: Query constructor for declared functions. 
      
       @param c logical context.
       @param constr constructor container. The container must have been passed in to a {!mk_datatype} call.
       @param num_fields number of accessor fields in the constructor.
       @param constructor constructor function declaration.
       @param tester constructor test function declaration.
       @param accessors array of accessor function declarations.
*)
val query_constructor : context -> constructor -> int -> func_decl * func_decl * func_decl array
	

*)
(**
       {2 {L Constants and Applications}}
*)
(**
       Summary: Declare a constant or function.

        [mk_func_decl c n d r] creates a function with name [n], domain [d], and range [r].
       The arity of the function is the size of the array [d]. 

       @param c logical context.
       @param s name of the constant or function.
       @param domain_size number of arguments. It is 0 when declaring a constant.
       @param domain array containing the sort of each argument. The array must contain domain_size elements. It is 0 when declaring a constant.
       @param range sort of the constant or the return sort of the function.

       After declaring a constant or function, the function
       {!mk_app} can be used to create a constant or function
       application.

       - {b See also}: {!mk_app}
*)
val mk_func_decl : context -> symbol -> sort array -> sort -> func_decl
	

(**
       Summary: Create a constant or function application.

       - {b See also}: {!mk_func_decl}
*)
val mk_app : context -> func_decl -> ast array -> ast
	

(**
       Summary: Declare and create a constant.
       
       
       
       
       
       
       
        [mk_const c s t] is a shorthand for [mk_app c (mk_func_decl c s [||] t) [||]] 

       - {b See also}: {!mk_func_decl}
       - {b See also}: {!mk_app}
*)
val mk_const : context -> symbol -> sort -> ast
	

(**
       Summary: Declare a fresh constant or function.

       Z3 will generate an unique name for this function declaration.
       
       
       

       - {b See also}: {!mk_func_decl}
*)
val mk_fresh_func_decl : context -> string -> sort array -> sort -> func_decl
	

(**
       Summary: Declare and create a fresh constant.
       
       
       

        [mk_fresh_const c p t] is a shorthand for [mk_app c (mk_fresh_func_decl c p [||] t) [||]]. 

       
       
       - {b See also}: {!mk_func_decl}
       - {b See also}: {!mk_app}
*)
val mk_fresh_const : context -> string -> sort -> ast
	

(**
       {2 {L Propositional Logic and Equality}}
*)
(**
        Summary: Create an AST node representing [true].
*)
val mk_true : context -> ast
	

(**
        Summary: Create an AST node representing [false].
*)
val mk_false : context -> ast
	

(**
        Summary: \[ [ mk_eq c l r ] \]
        Create an AST node representing {e l = r }.
        
        The nodes [l] and [r] must have the same type. 
*)
val mk_eq : context -> ast -> ast -> ast
	

(**
       
        Summary: \[ [mk_distinct c [| t_1; ...; t_n |]] \] Create an AST
       node represeting a distinct construct. It is used for declaring
       the arguments t_i pairwise distinct. 

       The [distinct] construct is used for declaring the arguments pairwise distinct.
       That is, {e Forall 0 <= i < j < num_args. not args[i] = args[j] }.
       
       All arguments must have the same sort.

       - {b Remarks}: The number of arguments of a distinct construct must be greater than one.
*)
val mk_distinct : context -> ast array -> ast
	

(**
        Summary: \[ [ mk_not c a ] \] 
        Create an AST node representing {e not(a) }.
        
        The node [a] must have Boolean sort.
*)
val mk_not : context -> ast -> ast
	

(**
       Summary: \[ [ mk_ite c t1 t2 t2 ] \] 
       Create an AST node representing an if-then-else: {e ite(t1, t2,
       t3) }.

       The node [t1] must have Boolean sort, [t2] and [t3] must have the same sort.
       The sort of the new node is equal to the sort of [t2] and [t3].
*)
val mk_ite : context -> ast -> ast -> ast -> ast
	

(**
       Summary: \[ [ mk_iff c t1 t2 ] \]
       Create an AST node representing {e t1 iff t2 }.

       The nodes [t1] and [t2] must have Boolean sort.
*)
val mk_iff : context -> ast -> ast -> ast
	

(**
       Summary: \[ [ mk_implies c t1 t2 ] \]
       Create an AST node representing {e t1 implies t2 }.

       The nodes [t1] and [t2] must have Boolean sort.
*)
val mk_implies : context -> ast -> ast -> ast
	

(**
       Summary: \[ [ mk_xor c t1 t2 ] \]
       Create an AST node representing {e t1 xor t2 }.

       The nodes [t1] and [t2] must have Boolean sort.
*)
val mk_xor : context -> ast -> ast -> ast
	

(**
       
        Summary: \[ [mk_and c [| t_1; ...; t_n |]] \] Create the conjunction: {e t_1 and ... and t_n}. 

       
       All arguments must have Boolean sort.
       
       - {b Remarks}: The number of arguments must be greater than zero.
*)
val mk_and : context -> ast array -> ast
	

(**
       
        Summary: \[ [mk_or c [| t_1; ...; t_n |]] \] Create the disjunction: {e t_1 or ... or t_n}. 

       
       All arguments must have Boolean sort.

       - {b Remarks}: The number of arguments must be greater than zero.
*)
val mk_or : context -> ast array -> ast
	

(**
       {2 {L Arithmetic: Integers and Reals}}
*)
(**
       
        Summary: \[ [mk_add c [| t_1; ...; t_n |]] \] Create the term: {e t_1 + ... + t_n}. 

       
       All arguments must have int or real sort.

       - {b Remarks}: The number of arguments must be greater than zero.
*)
val mk_add : context -> ast array -> ast
	

(**
       
        Summary: \[ [mk_mul c [| t_1; ...; t_n |]] \] Create the term: {e t_1 * ... * t_n}. 

       
       All arguments must have int or real sort.
       
       - {b Remarks}: Z3 has limited support for non-linear arithmetic.
       - {b Remarks}: The number of arguments must be greater than zero.
*)
val mk_mul : context -> ast array -> ast
	

(**
       
        Summary: \[ [mk_sub c [| t_1; ...; t_n |]] \] Create the term: {e t_1 - ... - t_n}. 

       
       All arguments must have int or real sort.

       - {b Remarks}: The number of arguments must be greater than zero.
*)
val mk_sub : context -> ast array -> ast
	

(**
       
        Summary: \[ [mk_unary_minus c arg] \] Create the term: {e - arg}. 

       The arguments must have int or real type.
*)
val mk_unary_minus : context -> ast -> ast
	

(**
       
        Summary: \[ [mk_div c t_1 t_2] \] Create the term: {e t_1 div t_2}. 

       The arguments must either both have int type or both have real type.
       If the arguments have int type, then the result type is an int type, otherwise the
       the result type is real.

*)
val mk_div : context -> ast -> ast -> ast
	

(**
       
        Summary: \[ [mk_mod c t_1 t_2] \] Create the term: {e t_1 mod t_2}. 

       The arguments must have int type.

*)
val mk_mod : context -> ast -> ast -> ast
	

(**
       
        Summary: \[ [mk_rem c t_1 t_2] \] Create the term: {e t_1 rem t_2}. 

       The arguments must have int type.

*)
val mk_rem : context -> ast -> ast -> ast
	

(**
       

       The arguments must have int or real type.
*)
val mk_power : context -> ast -> ast -> ast
	

(**
        Summary: \[ [ mk_lt c t1 t2 ] \] 
        Create less than.

        The nodes [t1] and [t2] must have the same sort, and must be int or real.
*)
val mk_lt : context -> ast -> ast -> ast
	

(**
        Summary: \[ [ mk_le c t1 t2 ] \]
        Create less than or equal to.
        
        The nodes [t1] and [t2] must have the same sort, and must be int or real.
*)
val mk_le : context -> ast -> ast -> ast
	

(**
        Summary: \[ [ mk_gt c t1 t2 ] \]
        Create greater than.
        
        The nodes [t1] and [t2] must have the same sort, and must be int or real.
*)
val mk_gt : context -> ast -> ast -> ast
	

(**
        Summary: \[ [ mk_ge c t1 t2 ] \]
        Create greater than or equal to.
        
        The nodes [t1] and [t2] must have the same sort, and must be int or real.
*)
val mk_ge : context -> ast -> ast -> ast
	

(**
        Summary: \[ [ mk_int2real c t1 ] \]
        Coerce an integer to a real.

        There is also a converse operation exposed.
        It follows the semantics prescribed by the SMT-LIB standard.

        You can take the floor of a real by 
        creating an auxiliary integer constant [k] and
        and asserting {e  mk_int2real(k) <= t1 < mk_int2real(k)+1 }.
        
        The node [t1] must have sort integer.

        - {b See also}: {!mk_real2int}
        - {b See also}: {!mk_is_int}
*)
val mk_int2real : context -> ast -> ast
	

(**
        Summary: \[ [ mk_real2int c t1 ] \]
        Coerce a real to an integer.

        The semantics of this function follows the SMT-LIB standard
        for the function to_int

        - {b See also}: {!mk_int2real}
        - {b See also}: {!mk_is_int}
*)
val mk_real2int : context -> ast -> ast
	

(**
        Summary: \[ [ mk_is_int c t1 ] \]
        Check if a real number is an integer.

        - {b See also}: {!mk_int2real}
        - {b See also}: {!mk_real2int}
*)
val mk_is_int : context -> ast -> ast
	

(**
       {2 {L Bit-vectors}}
*)
(**
       Summary: \[ [ mk_bvnot c t1 ] \]
       Bitwise negation.

       The node [t1] must have a bit-vector sort.
*)
val mk_bvnot : context -> ast -> ast
	

(**
       Summary: \[ [ mk_bvredand c t1 ] \]
       Take conjunction of bits in vector, return vector of length 1.

       The node [t1] must have a bit-vector sort.
*)
val mk_bvredand : context -> ast -> ast
	

(**
       Summary: \[ [ mk_bvredor c t1 ] \]
       Take disjunction of bits in vector, return vector of length 1.

       The node [t1] must have a bit-vector sort.
*)
val mk_bvredor : context -> ast -> ast
	

(**
       Summary: \[ [ mk_bvand c t1 t2 ] \]
       Bitwise and.

       The nodes [t1] and [t2] must have the same bit-vector sort.
*)
val mk_bvand : context -> ast -> ast -> ast
	

(**
       Summary: \[ [ mk_bvor c t1 t2 ] \]
       Bitwise or.

       The nodes [t1] and [t2] must have the same bit-vector sort.
*)
val mk_bvor : context -> ast -> ast -> ast
	

(**
       Summary: \[ [ mk_bvxor c t1 t2 ] \]
       Bitwise exclusive-or.

       The nodes [t1] and [t2] must have the same bit-vector sort.
*)
val mk_bvxor : context -> ast -> ast -> ast
	

(**
       Summary: \[ [ mk_bvnand c t1 t2 ] \]
       Bitwise nand. 

       The nodes [t1] and [t2] must have the same bit-vector sort.
*)
val mk_bvnand : context -> ast -> ast -> ast
	

(**
       Summary: \[ [ mk_bvnor c t1 t2 ] \]
       Bitwise nor. 

       The nodes [t1] and [t2] must have the same bit-vector sort.
*)
val mk_bvnor : context -> ast -> ast -> ast
	

(**
       Summary: \[ [ mk_bvxnor c t1 t2 ] \]
       Bitwise xnor. 
       
       The nodes [t1] and [t2] must have the same bit-vector sort.
*)
val mk_bvxnor : context -> ast -> ast -> ast
	

(**
       Summary: \[ [ mk_bvneg c t1 ] \]
       Standard two's complement unary minus. 

       The node [t1] must have bit-vector sort.
*)
val mk_bvneg : context -> ast -> ast
	

(**
        Summary: \[ [ mk_bvadd c t1 t2 ] \]
        Standard two's complement addition.
        
        The nodes [t1] and [t2] must have the same bit-vector sort.
*)
val mk_bvadd : context -> ast -> ast -> ast
	

(**
        Summary: \[ [ mk_bvsub c t1 t2 ] \]
        Standard two's complement subtraction.
        
        The nodes [t1] and [t2] must have the same bit-vector sort.
*)
val mk_bvsub : context -> ast -> ast -> ast
	

(**
        Summary: \[ [ mk_bvmul c t1 t2 ] \]
        Standard two's complement multiplication.
        
        The nodes [t1] and [t2] must have the same bit-vector sort.
*)
val mk_bvmul : context -> ast -> ast -> ast
	

(**
        Summary: \[ [ mk_bvudiv c t1 t2 ] \]
        Unsigned division. 

        It is defined as the [floor] of {e t1/t2 } if [t2] is
        different from zero. If {e t2 } is zero, then the result
        is undefined.
        
        The nodes [t1] and [t2] must have the same bit-vector sort.
*)
val mk_bvudiv : context -> ast -> ast -> ast
	

(**
        Summary: \[ [ mk_bvsdiv c t1 t2 ] \]
        Two's complement signed division. 

        It is defined in the following way:

        - The [floor] of {e t1/t2 } if [t2] is different from zero, and {e t1*t2 >= 0 }.

        - The [ceiling] of {e t1/t2 } if [t2] is different from zero, and {e t1*t2 < 0 }.
        
        If {e t2 } is zero, then the result is undefined.
        
        The nodes [t1] and [t2] must have the same bit-vector sort.
*)
val mk_bvsdiv : context -> ast -> ast -> ast
	

(**
       Summary: \[ [ mk_bvurem c t1 t2 ] \]
       Unsigned remainder.

       It is defined as {e t1 - (t1 /u t2) * t2 }, where {e /u } represents unsigned int division.
       
       If {e t2 } is zero, then the result is undefined.
       
       The nodes [t1] and [t2] must have the same bit-vector sort.
*)
val mk_bvurem : context -> ast -> ast -> ast
	

(**
       Summary: \[ [ mk_bvsrem c t1 t2 ] \]
       Two's complement signed remainder (sign follows dividend).

       It is defined as {e t1 - (t1 /s t2) * t2 }, where {e /s } represents signed division.
       The most significant bit (sign) of the result is equal to the most significant bit of [t1].

       If {e t2 } is zero, then the result is undefined.
       
       The nodes [t1] and [t2] must have the same bit-vector sort.

       - {b See also}: {!mk_bvsmod}
*)
val mk_bvsrem : context -> ast -> ast -> ast
	

(**
       Summary: \[ [ mk_bvsmod c t1 t2 ] \]
       Two's complement signed remainder (sign follows divisor).
       
       If {e t2 } is zero, then the result is undefined.
       
       The nodes [t1] and [t2] must have the same bit-vector sort.

       - {b See also}: {!mk_bvsrem}
*)
val mk_bvsmod : context -> ast -> ast -> ast
	

(**
       Summary: \[ [ mk_bvult c t1 t2 ] \]
       Unsigned less than.

       The nodes [t1] and [t2] must have the same bit-vector sort.
*)
val mk_bvult : context -> ast -> ast -> ast
	

(**
       Summary: \[ [ mk_bvslt c t1 t2 ] \]
       Two's complement signed less than.
       
       It abbreviates:
       {v 
      (or (and (= (extract[|m-1|:|m-1|] t1) bit1)
               (= (extract[|m-1|:|m-1|] t2) bit0))
          (and (= (extract[|m-1|:|m-1|] t1) (extract[|m-1|:|m-1|] t2))
               (bvult t1 t2)))
        v}

       The nodes [t1] and [t2] must have the same bit-vector sort.
*)
val mk_bvslt : context -> ast -> ast -> ast
	

(**
       Summary: \[ [ mk_bvule c t1 t2 ] \]
       Unsigned less than or equal to.

       The nodes [t1] and [t2] must have the same bit-vector sort.
*)
val mk_bvule : context -> ast -> ast -> ast
	

(**
       Summary: \[ [ mk_bvsle c t1 t2 ] \]
       Two's complement signed less than or equal to.

       The nodes [t1] and [t2] must have the same bit-vector sort.
*)
val mk_bvsle : context -> ast -> ast -> ast
	

(**
       Summary: \[ [ mk_bvuge c t1 t2 ] \]
       Unsigned greater than or equal to.

       The nodes [t1] and [t2] must have the same bit-vector sort.
*)
val mk_bvuge : context -> ast -> ast -> ast
	

(**
       Summary: \[ [ mk_bvsge c t1 t2 ] \]
       Two's complement signed greater than or equal to.

       The nodes [t1] and [t2] must have the same bit-vector sort.
*)
val mk_bvsge : context -> ast -> ast -> ast
	

(**
       Summary: \[ [ mk_bvugt c t1 t2 ] \]
       Unsigned greater than.

       The nodes [t1] and [t2] must have the same bit-vector sort.
*)
val mk_bvugt : context -> ast -> ast -> ast
	

(**
       Summary: \[ [ mk_bvsgt c t1 t2 ] \]
       Two's complement signed greater than.

       The nodes [t1] and [t2] must have the same bit-vector sort.
*)
val mk_bvsgt : context -> ast -> ast -> ast
	

(**
       Summary: \[ [ mk_concat c t1 t2 ] \]
       Concatenate the given bit-vectors.
       
       The nodes [t1] and [t2] must have (possibly different) bit-vector sorts

       The result is a bit-vector of size {e n1+n2 }, where [n1] ([n2)] is the size
       of [t1] ([t2)].
*)
val mk_concat : context -> ast -> ast -> ast
	

(**
       Summary: \[ [ mk_extract c high low t1 ] \]
       Extract the bits [high] down to [low] from a bitvector of
       size [m] to yield a new bitvector of size [n], where {e n =
       high - low + 1 }.

       The node [t1] must have a bit-vector sort.
*)
val mk_extract : context -> int -> int -> ast -> ast
	

(**
       Summary: \[ [ mk_sign_ext c i t1 ] \]
       Sign-extend of the given bit-vector to the (signed) equivalent bitvector of
       size {e m+i }, where [m] is the size of the given
       bit-vector.

       The node [t1] must have a bit-vector sort.
*)
val mk_sign_ext : context -> int -> ast -> ast
	

(**
       Summary: \[ [ mk_zero_ext c i t1 ] \]
       Extend the given bit-vector with zeros to the (unsigned int) equivalent
       bitvector of size {e m+i }, where [m] is the size of the
       given bit-vector.
       
       The node [t1] must have a bit-vector sort. 
*)
val mk_zero_ext : context -> int -> ast -> ast
	

(**
       Summary: \[ [ mk_repeat c i t1 ] \]
       Repeat the given bit-vector up length {e i }.
       
       The node [t1] must have a bit-vector sort. 
*)
val mk_repeat : context -> int -> ast -> ast
	

(**
       Summary: \[ [ mk_bvshl c t1 t2 ] \]
       Shift left.

       It is equivalent to multiplication by {e 2^x } where [x] is the value of the
       third argument.

       NB. The semantics of shift operations varies between environments. This 
       definition does not necessarily capture directly the semantics of the 
       programming language or assembly architecture you are modeling.

       The nodes [t1] and [t2] must have the same bit-vector sort.
*)
val mk_bvshl : context -> ast -> ast -> ast
	

(**
       Summary: \[ [ mk_bvlshr c t1 t2 ] \]
       Logical shift right.

       It is equivalent to unsigned int division by {e 2^x } where [x] is the
       value of the third argument.

       NB. The semantics of shift operations varies between environments. This 
       definition does not necessarily capture directly the semantics of the 
       programming language or assembly architecture you are modeling.

       The nodes [t1] and [t2] must have the same bit-vector sort.
*)
val mk_bvlshr : context -> ast -> ast -> ast
	

(**
       Summary: \[ [ mk_bvashr c t1 t2 ] \]
       Arithmetic shift right.
       
       It is like logical shift right except that the most significant
       bits of the result always copy the most significant bit of the
       second argument.

       NB. The semantics of shift operations varies between environments. This 
       definition does not necessarily capture directly the semantics of the 
       programming language or assembly architecture you are modeling.
       
       The nodes [t1] and [t2] must have the same bit-vector sort.
*)
val mk_bvashr : context -> ast -> ast -> ast
	

(**
       Summary: \[ [ mk_rotate_left c i t1 ] \]
       Rotate bits of [t1] to the left [i] times.
       
       The node [t1] must have a bit-vector sort. 
*)
val mk_rotate_left : context -> int -> ast -> ast
	

(**
       Summary: \[ [ mk_rotate_right c i t1 ] \]
       Rotate bits of [t1] to the right [i] times.
       
       The node [t1] must have a bit-vector sort. 
*)
val mk_rotate_right : context -> int -> ast -> ast
	

(**
       Summary: \[ [ mk_ext_rotate_left c t1 t2 ] \]
       Rotate bits of [t1] to the left [t2] times.
       
       The nodes [t1] and [t2] must have the same bit-vector sort.
*)
val mk_ext_rotate_left : context -> ast -> ast -> ast
	

(**
       Summary: \[ [ mk_ext_rotate_right c t1 t2 ] \]
       Rotate bits of [t1] to the right [t2] times.
       
       The nodes [t1] and [t2] must have the same bit-vector sort.
*)
val mk_ext_rotate_right : context -> ast -> ast -> ast
	

(**
       Summary: \[ [ mk_int2bv c n t1 ] \]
       Create an [n] bit bit-vector from the integer argument [t1].

       NB. This function is essentially treated as uninterpreted. 
       So you cannot expect Z3 to precisely reflect the semantics of this function
       when solving constraints with this function.
       
       The node [t1] must have integer sort. 
*)
val mk_int2bv : context -> int -> ast -> ast
	

(**
       Summary: \[ [ mk_bv2int c t1 is_signed ] \]
       Create an integer from the bit-vector argument [t1].
       If [is_signed] is false, then the bit-vector [t1] is treated as unsigned int. 
       So the result is non-negative
       and in the range {e [0..2^N-1] }, where N are the number of bits in [t1].
       If [is_signed] is true, [t1] is treated as a signed bit-vector.

       NB. This function is essentially treated as uninterpreted. 
       So you cannot expect Z3 to precisely reflect the semantics of this function
       when solving constraints with this function.

       The node [t1] must have a bit-vector sort. 
*)
val mk_bv2int : context -> ast -> bool -> ast
	

(**
       Summary: \[ [ mk_bvadd_no_overflow c t1 t2 is_signed ] \]
       Create a predicate that checks that the bit-wise addition
       of [t1] and [t2] does not overflow.
       
       The nodes [t1] and [t2] must have the same bit-vector sort.
*)
val mk_bvadd_no_overflow : context -> ast -> ast -> bool -> ast
	

(**
       Summary: \[ [ mk_bvadd_no_underflow c t1 t2 ] \]
       Create a predicate that checks that the bit-wise signed addition
       of [t1] and [t2] does not underflow.
       
       The nodes [t1] and [t2] must have the same bit-vector sort.
*)
val mk_bvadd_no_underflow : context -> ast -> ast -> ast
	

(**
       Summary: \[ [ mk_bvsub_no_overflow c t1 t2 ] \]
       Create a predicate that checks that the bit-wise signed subtraction
       of [t1] and [t2] does not overflow.
       
       The nodes [t1] and [t2] must have the same bit-vector sort.
*)
val mk_bvsub_no_overflow : context -> ast -> ast -> ast
	

(**
       Summary: \[ [ mk_bvsub_no_underflow c t1 t2 is_signed ] \]
       Create a predicate that checks that the bit-wise subtraction
       of [t1] and [t2] does not underflow.
       
       The nodes [t1] and [t2] must have the same bit-vector sort.
*)
val mk_bvsub_no_underflow : context -> ast -> ast -> bool -> ast
	

(**
       Summary: \[ [ mk_bvsdiv_no_overflow c t1 t2 ] \]
       Create a predicate that checks that the bit-wise signed division 
       of [t1] and [t2] does not overflow.
       
       The nodes [t1] and [t2] must have the same bit-vector sort.
*)
val mk_bvsdiv_no_overflow : context -> ast -> ast -> ast
	

(**
       Summary: \[ [ mk_bvneg_no_overflow c t1 ] \]
       Check that bit-wise negation does not overflow when 
       [t1] is interpreted as a signed bit-vector.
       
       The node [t1] must have bit-vector sort.
*)
val mk_bvneg_no_overflow : context -> ast -> ast
	

(**
       Summary: \[ [ mk_bvmul_no_overflow c t1 t2 is_signed ] \]
       Create a predicate that checks that the bit-wise multiplication
       of [t1] and [t2] does not overflow.
       
       The nodes [t1] and [t2] must have the same bit-vector sort.
*)
val mk_bvmul_no_overflow : context -> ast -> ast -> bool -> ast
	

(**
       Summary: \[ [ mk_bvmul_no_underflow c t1 t2 ] \]
       Create a predicate that checks that the bit-wise signed multiplication
       of [t1] and [t2] does not underflow.
       
       The nodes [t1] and [t2] must have the same bit-vector sort.
*)
val mk_bvmul_no_underflow : context -> ast -> ast -> ast
	

(**
       {2 {L Arrays}}
*)
(**
       Summary: \[ [ mk_select c a i ] \]
       Array read.
       The argument [a] is the array and [i] is the index of the array that gets read.      
 
       The node [a] must have an array sort {e [domain -> range] }, 
       and [i] must have the sort [domain].
       The sort of the result is [range].

       - {b See also}: {!mk_array_sort}
       - {b See also}: {!mk_store}
*)
val mk_select : context -> ast -> ast -> ast
	

(**
       Summary: \[ [ mk_store c a i v ] \]
       Array update.
       
       The node [a] must have an array sort {e [domain -> range] }, [i] must have sort [domain],
       [v] must have sort range. The sort of the result is {e [domain -> range] }.
       The semantics of this function is given by the theory of arrays described in the SMT-LIB
       standard. See http:
       The result of this function is an array that is equal to [a] (with respect to [select)]
       on all indices except for [i], where it maps to [v] (and the [select] of [a] with 
       respect to [i] may be a different value).
       
       - {b See also}: {!mk_array_sort}
       - {b See also}: {!mk_select}
*)
val mk_store : context -> ast -> ast -> ast -> ast
	

(**
        Summary: Create the constant array.
         
        The resulting term is an array, such that a [select] on an arbitrary index 
        produces the value [v].

        @param c logical context.
        @param domain domain sort for the array.
        @param v value that the array maps to.
*)
val mk_const_array : context -> sort -> ast -> ast
	

(**
       Summary: \[ [ mk_map f n args ] \]
       map f on the the argument arrays.
       
       The [n] nodes [args] must be of array sorts {e [domain_i -> range_i] }.
       The function declaration [f] must have type {e  range_1 .. range_n -> range }.
       [v] must have sort range. The sort of the result is {e [domain_i -> range] }.
       
       - {b See also}: {!mk_array_sort}
       - {b See also}: {!mk_store}
       - {b See also}: {!mk_select}
*)
val mk_map : context -> func_decl -> int -> ast -> ast
	

(**
        Summary: Access the array default value.
        Produces the default range value, for arrays that can be represented as 
        finite maps with a default range value.

        @param c logical context.
        @param array array value whose default range value is accessed.

*)
val mk_array_default : context -> ast -> ast
	

(**
       {2 {L Sets}}
*)
(**
       Summary: Create Set type.
*)
val mk_set_sort : context -> sort -> sort
	

(**
        Summary: Create the empty set.
*)
val mk_empty_set : context -> sort -> ast
	

(**
        Summary: Create the full set.
*)
val mk_full_set : context -> sort -> ast
	

(**
       Summary: Add an element to a set.
       
       The first argument must be a set, the second an element.
*)
val mk_set_add : context -> ast -> ast -> ast
	

(**
       Summary: Remove an element to a set.
       
       The first argument must be a set, the second an element.
*)
val mk_set_del : context -> ast -> ast -> ast
	

(**
       Summary: Take the union of a list of sets.
*)
val mk_set_union : context -> ast array -> ast
	

(**
       Summary: Take the intersection of a list of sets.
*)
val mk_set_intersect : context -> ast array -> ast
	

(**
       Summary: Take the set difference between two sets.
*)
val mk_set_difference : context -> ast -> ast -> ast
	

(**
       Summary: Take the complement of a set.
*)
val mk_set_complement : context -> ast -> ast
	

(**
       Summary: Check for set membership.
       
       The first argument should be an element type of the set.
*)
val mk_set_member : context -> ast -> ast -> ast
	

(**
       Summary: Check for subsetness of sets.
*)
val mk_set_subset : context -> ast -> ast -> ast
	

(**
       {2 {L Numerals}}
*)

(**
  Summary: \[ [ numeral_refined ] \] is the refined view of a numeral .
*)
type numeral_refined =
  | Numeral_int      of int * sort
  | Numeral_int64    of int64 * sort
  | Numeral_large    of string * sort
  | Numeral_rational of numeral_refined * numeral_refined


(**
   Summary: \[ [ embed_numeral c nr ] \] constructs the numeral described by [nr].

   - {b See also}: {!numeral_refine}
*)
val embed_numeral: context -> numeral_refined -> ast

(**
        {4 {L Redundant low-level API}} 
*)
(**
       Summary: Create a numeral of a given sort. 

       @param c logical context.
       @param numeral A string representing the numeral value in decimal notation. If the given sort is a real, then the numeral can be a rational, that is, a string of the form {e [num]* / [num]* }.
       @param ty The sort of the numeral. In the current implementation, the given sort can be an int, real, finite-domain, or bit-vectors of arbitrary size. 
       
       - {b See also}: {!mk_int}
       
*)
val mk_numeral : context -> string -> sort -> ast
	

(**
       Summary: Create a real from a fraction.

       @param c logical context.
       @param num numerator of rational.
       @param den denomerator of rational.

       - {b Precondition}: den != 0

       - {b See also}: {!mk_numeral}
       - {b See also}: {!mk_int}
       
*)
val mk_real : context -> int -> int -> ast
	

(**
       Summary: Create a numeral of an int, bit-vector, or finite-domain sort. 
       
       This function can be use to create numerals that fit in a machine integer.
       It is slightly faster than {!mk_numeral} since it is not necessary to parse a string.

       - {b See also}: {!mk_numeral}
*)
val mk_int : context -> int -> sort -> ast
	

(**
       Summary: Create a numeral of a int, bit-vector, or finite-domain sort. 
       
       This function can be use to create numerals that fit in a machine long long integer.
       It is slightly faster than {!mk_numeral} since it is not necessary to parse a string.

       - {b See also}: {!mk_numeral}
*)
val mk_int64 : context -> int64 -> sort -> ast
	

(**
       {2 {L Quantifiers}}
*)
(**
       Summary: Create a pattern for quantifier instantiation.

       Z3 uses pattern matching to instantiate quantifiers. If a
       pattern is not provided for a quantifier, then Z3 will
       automatically compute a set of patterns for it. However, for
       optimal performance, the user should provide the patterns.

       Patterns comprise a list of terms. The list should be
       non-empty.  If the list comprises of more than one term, it is
       a called a multi-pattern.
       
       In general, one can pass in a list of (multi-)patterns in the
       quantifier constructor.


       - {b See also}: {!mk_forall}
       - {b See also}: {!mk_exists}
*)
val mk_pattern : context -> ast array -> pattern
	

(**
       Summary: Create a bound variable.

       Bound variables are indexed by de-Bruijn indices. It is perhaps easiest to explain
       the meaning of de-Bruijn indices by indicating the compilation process from
       non-de-Bruijn formulas to de-Bruijn format.

       {v  
       abs(forall (x1) phi) = forall (x1) abs1(phi, x1, 0)
       abs(forall (x1, x2) phi) = abs(forall (x1) abs(forall (x2) phi))
       abs1(x, x, n) = b_n
       abs1(y, x, n) = y
       abs1(f(t1,...,tn), x, n) = f(abs1(t1,x,n), ..., abs1(tn,x,n))
       abs1(forall (x1) phi, x, n) = forall (x1) (abs1(phi, x, n+1))
        v}

       The last line is significant: the index of a bound variable is different depending
       on the scope in which it appears. The deeper x appears, the higher is its
       index.
       
       @param c logical context
       @param index de-Bruijn index
       @param ty sort of the bound variable

       - {b See also}: {!mk_forall}
       - {b See also}: {!mk_exists}
*)
val mk_bound : context -> int -> sort -> ast
	

(**
       Summary: Create a forall formula. It takes an expression [body] that contains bound variables
       of the same sorts as the sorts listed in the array [sorts]. The bound variables are de-Bruijn indices created
       using {!mk_bound}. The array [decl_names] contains the names that the quantified formula uses for the 
       bound variables. Z3 applies the convention that the last element in the [decl_names] and [sorts] array
       refers to the variable with index 0, the second to last element of [decl_names] and [sorts] refers
       to the variable with index 1, etc.
       

        [mk_forall c w p t n b] creates a forall formula, where
       [w] is the weight, [p] is an array of patterns, [t] is an array
       with the sorts of the bound variables, [n] is an array with the
       'names' of the bound variables, and [b] is the body of the
       quantifier. Quantifiers are associated with weights indicating
       the importance of using the quantifier during
       instantiation. 
       
       
       @param c logical context.
       @param weight quantifiers are associated with weights indicating the importance of using the quantifier during instantiation. By default, pass the weight 0.
       @param num_patterns number of patterns.
       @param patterns array containing the patterns created using {!mk_pattern}.
       @param num_decls number of variables to be bound.
       @param sorts the sorts of the bound variables.
       @param decl_names names of the bound variables
       @param body the body of the quantifier.
       
       - {b See also}: {!mk_pattern}
       - {b See also}: {!mk_bound}
       - {b See also}: {!mk_exists}
*)
val mk_forall : context -> int -> pattern array -> sort array -> symbol array -> ast -> ast
	

(**
       Summary: Create an exists formula. Similar to {!mk_forall}.
       
       - {b See also}: {!mk_pattern}
       - {b See also}: {!mk_bound}
       - {b See also}: {!mk_forall}
       - {b See also}: {!mk_quantifier}
*)
val mk_exists : context -> int -> pattern array -> sort array -> symbol array -> ast -> ast
	

(**
       Summary: Create a quantifier - universal or existential, with pattern hints. 
       See the documentation for {!mk_forall} for an explanation of the parameters.
       
       @param c logical context.
       @param is_forall flag to indicate if this is a universal or existential quantifier.
       @param weight quantifiers are associated with weights indicating the importance of using the quantifier during instantiation. By default, pass the weight 0.
       @param num_patterns number of patterns.
       @param patterns array containing the patterns created using {!mk_pattern}.
       @param num_decls number of variables to be bound.
       @param sorts array of sorts of the bound variables.
       @param decl_names names of the bound variables.
       @param body the body of the quantifier.
       
       - {b See also}: {!mk_pattern}
       - {b See also}: {!mk_bound}
       - {b See also}: {!mk_forall}
       - {b See also}: {!mk_exists}
*)
val mk_quantifier : context -> bool -> int -> pattern array -> sort array -> symbol array -> ast -> ast
	

(**
       Summary: Create a quantifier - universal or existential, with pattern hints, no patterns, and attributes
       
       @param c logical context.
       @param is_forall flag to indicate if this is a universal or existential quantifier.
       @param quantifier_id identifier to identify quantifier
       @param skolem_id identifier to identify skolem constants introduced by quantifier.
       @param weight quantifiers are associated with weights indicating the importance of using the quantifier during instantiation. By default, pass the weight 0.
       @param num_patterns number of patterns.
       @param patterns array containing the patterns created using {!mk_pattern}.
       @param num_no_patterns number of patterns.
       @param no_patterns array containing the patterns created using {!mk_pattern}.
       @param num_decls number of variables to be bound.
       @param sorts array of sorts of the bound variables.
       @param decl_names names of the bound variables.
       @param body the body of the quantifier.
       
       - {b See also}: {!mk_pattern}
       - {b See also}: {!mk_bound}
       - {b See also}: {!mk_forall}
       - {b See also}: {!mk_exists}
*)
val mk_quantifier_ex : context -> bool -> int -> symbol -> symbol -> pattern array -> ast array -> sort array -> symbol array -> ast -> ast
	

(**
       Summary: Create a universal quantifier using a list of constants that
       will form the set of bound variables.

       @param c logical context.
       @param weight quantifiers are associated with weights indicating the importance of using 
              the quantifier during instantiation. By default, pass the weight 0.
       @param num_bound number of constants to be abstracted into bound variables.
       @param bound array of constants to be abstracted into bound variables.
       @param num_patterns number of patterns.
       @param patterns array containing the patterns created using {!mk_pattern}.
       @param body the body of the quantifier.
       
       - {b See also}: {!mk_pattern}
       - {b See also}: {!mk_exists_const}

*)
val mk_forall_const : context -> int -> app array -> pattern array -> ast -> ast
	

(**
       Summary: Similar to {!mk_forall_const}.

       Summary: Create an existential quantifier using a list of constants that
       will form the set of bound variables.

       @param c logical context.
       @param weight quantifiers are associated with weights indicating the importance of using 
              the quantifier during instantiation. By default, pass the weight 0.
       @param num_bound number of constants to be abstracted into bound variables.
       @param bound array of constants to be abstracted into bound variables.
       @param num_patterns number of patterns.
       @param patterns array containing the patterns created using {!mk_pattern}.
       @param body the body of the quantifier.
       
       - {b See also}: {!mk_pattern}
       - {b See also}: {!mk_forall_const}
*)
val mk_exists_const : context -> int -> app array -> pattern array -> ast -> ast
	

(**
       Summary: Create a universal or existential 
       quantifier using a list of constants that
       will form the set of bound variables.
*)
val mk_quantifier_const : context -> bool -> int -> app array -> pattern array -> ast -> ast
	

(**
       Summary: Create a universal or existential 
       quantifier using a list of constants that
       will form the set of bound variables.
*)
val mk_quantifier_const_ex : context -> bool -> int -> symbol -> symbol -> app array -> pattern array -> ast array -> ast -> ast
	

(**
       {2 {L Accessors}}
*)
(**
        {3 {L Symbols}} 
*)

(**
  Summary: \[ [ symbol_refine c s ] \] is the refined view of [s].
*)
val symbol_refine: context -> symbol -> symbol_refined

(**
        {4 {L Redundant low-level API}} 
*)
(**
       Summary: Return [INT_SYMBOL] if the symbol was constructed
       using {!mk_int_symbol}, and [STRING_SYMBOL] if the symbol
       was constructed using {!mk_string_symbol}.
*)
val get_symbol_kind : context -> symbol -> symbol_kind
	

(**
       Summary: \[ [ get_symbol_int c s ] \]
       Return the symbol int value. 
       
       - {b Precondition}: get_symbol_kind s == INT_SYMBOL

       - {b See also}: {!mk_int_symbol}
*)
val get_symbol_int : context -> symbol -> int
	

(**
       Summary: \[ [ get_symbol_string c s ] \]
       Return the symbol name. 

       - {b Precondition}: get_symbol_string s == STRING_SYMBOL

       
       
       

       - {b See also}: {!mk_string_symbol}
*)
val get_symbol_string : context -> symbol -> string
	

(**
        {3 {L Sorts}} 
*)

(**
   Summary: \[ [ sort_refine c s ] \] is the refined view of [s].
*)
val sort_refine: context -> sort -> sort_refined

(**
       Summary: Return the sort name as a symbol. 
*)
val get_sort_name : context -> sort -> symbol
	

(**
        Summary: Return a unique identifier for [s].
         - {b Remarks}: Implicitly used by [Pervasives.( = )] and [Pervasives.compare]. 
*)
val get_sort_id : context -> sort -> int
	

(**
        {4 {L Redundant low-level API}} 
*)
(**
       Summary: Convert a [sort] into [ast]. 
        - {b Remarks}: [sort_to_ast c s] can be replaced by [(s :> ast)]. 
*)
val sort_to_ast : context -> sort -> ast
	

(**
       Summary: compare sorts.
        - {b Remarks}: [Pervasives.( = )] or [Pervasives.compare] can also be used. 
*)
val is_eq_sort : context -> sort -> sort -> bool
	

(**
       Summary: Return the sort kind (e.g., array, tuple, int, bool, etc).

       - {b See also}: {!sort_kind}
*)
val get_sort_kind : context -> sort -> sort_kind
	

(**
       Summary: \[ [ get_bv_sort_size c t ] \]
       Return the size of the given bit-vector sort. 

       - {b Precondition}: get_sort_kind c t == BV_SORT

       - {b See also}: {!mk_bv_sort}
       - {b See also}: {!get_sort_kind}
*)
val get_bv_sort_size : context -> sort -> int
	

(**
        
         Summary: Return the size of the sort in [r].  Return [None] if the call failed. 
        That is, get_sort_kind(s) == FINITE_DOMAIN_SORT
*)
val get_finite_domain_sort_size : context -> sort -> int64 option
	

(**
       Summary: \[ [ get_array_sort_domain c t ] \]
       Return the domain of the given array sort.
       
       - {b Precondition}: get_sort_kind c t == ARRAY_SORT

       - {b See also}: {!mk_array_sort}
       - {b See also}: {!get_sort_kind}
*)
val get_array_sort_domain : context -> sort -> sort
	

(**
       Summary: \[ [ get_array_sort_range c t ] \] 
       Return the range of the given array sort. 

       - {b Precondition}: get_sort_kind c t == ARRAY_SORT

       - {b See also}: {!mk_array_sort}
       - {b See also}: {!get_sort_kind}
*)
val get_array_sort_range : context -> sort -> sort
	

(**
       Summary: \[ [ get_tuple_sort_mk_decl c t ] \]
       Return the constructor declaration of the given tuple
       sort. 

       - {b Precondition}: get_sort_kind c t == DATATYPE_SORT

       - {b See also}: {!mk_tuple_sort}
       - {b See also}: {!get_sort_kind}
*)
val get_tuple_sort_mk_decl : context -> sort -> func_decl
	

(**
       Summary: \[ [ get_tuple_sort_num_fields c t ] \]
       Return the number of fields of the given tuple sort. 

       - {b Precondition}: get_sort_kind c t == DATATYPE_SORT

       - {b See also}: {!mk_tuple_sort}
       - {b See also}: {!get_sort_kind}
*)
val get_tuple_sort_num_fields : context -> sort -> int
	

(**
       Summary: \[ [ get_tuple_sort_field_decl c t i ] \]
       Return the i-th field declaration (i.e., projection function declaration)
       of the given tuple sort. 

       - {b Precondition}: get_sort_kind t == DATATYPE_SORT
       - {b Precondition}: i < get_tuple_sort_num_fields c t
       
       - {b See also}: {!mk_tuple_sort}
       - {b See also}: {!get_sort_kind}
*)
val get_tuple_sort_field_decl : context -> sort -> int -> func_decl
	

(**
        Summary: Return number of constructors for datatype.

        - {b Precondition}: get_sort_kind t == DATATYPE_SORT

        - {b See also}: {!get_datatype_sort_constructor}
        - {b See also}: {!get_datatype_sort_recognizer}
        - {b See also}: {!get_datatype_sort_constructor_accessor}

*)
val get_datatype_sort_num_constructors : context -> sort -> int
	

(**
        Summary: Return idx'th constructor.

        - {b Precondition}: get_sort_kind t == DATATYPE_SORT
        - {b Precondition}: idx < get_datatype_sort_num_constructors c t

        - {b See also}: {!get_datatype_sort_num_constructors}
        - {b See also}: {!get_datatype_sort_recognizer}
        - {b See also}: {!get_datatype_sort_constructor_accessor}

*)
val get_datatype_sort_constructor : context -> sort -> int -> func_decl
	

(**
        Summary: Return idx'th recognizer.

        - {b Precondition}: get_sort_kind t == DATATYPE_SORT
        - {b Precondition}: idx < get_datatype_sort_num_constructors c t

        - {b See also}: {!get_datatype_sort_num_constructors}
        - {b See also}: {!get_datatype_sort_constructor}
        - {b See also}: {!get_datatype_sort_constructor_accessor}

*)
val get_datatype_sort_recognizer : context -> sort -> int -> func_decl
	

(**
        Summary: Return idx_a'th accessor for the idx_c'th constructor.

        - {b Precondition}: get_sort_kind t == DATATYPE_SORT
        - {b Precondition}: idx_c < get_datatype_sort_num_constructors c t
        - {b Precondition}: idx_a < get_domain_size c get_datatype_sort_constructor c idx_c

        - {b See also}: {!get_datatype_sort_num_constructors}
        - {b See also}: {!get_datatype_sort_constructor}
        - {b See also}: {!get_datatype_sort_recognizer}
*)
val get_datatype_sort_constructor_accessor : context -> sort -> int -> int -> func_decl
	

(**
        Summary: Return arity of relation.

        - {b Precondition}: get_sort_kind s == RELATION_SORT

        - {b See also}: {!get_relation_column}
*)
val get_relation_arity : context -> sort -> int
	

(**
        Summary: Return sort at i'th column of relation sort.

        - {b Precondition}: get_sort_kind c s == RELATION_SORT
        - {b Precondition}: col < get_relation_arity c s

        - {b See also}: {!get_relation_arity}
*)
val get_relation_column : context -> sort -> int -> sort
	

(**
        {3 {L Function Declarations}} 
*)
(**
       Summary: Convert a [func_decl] into [ast]. 
        - {b Remarks}: [func_decl_to_ast c f]  can be replaced by [(f :> ast)]. 
*)
val func_decl_to_ast : context -> func_decl -> ast
	

(**
       Summary: compare terms.
        - {b Remarks}: [Pervasives.( = )] or [Pervasives.compare] can also be used. 
*)
val is_eq_func_decl : context -> func_decl -> func_decl -> bool
	

(**
        Summary: Return a unique identifier for [f].
         - {b Remarks}: Implicitly used by [Pervasives.( = )] and [Pervasives.compare]. 
*)
val get_func_decl_id : context -> func_decl -> int
	

(**
       Summary: Return the constant declaration name as a symbol. 
*)
val get_decl_name : context -> func_decl -> symbol
	

(**
       Summary: Return declaration kind corresponding to declaration.
*)
val get_decl_kind : context -> func_decl -> decl_kind
	

(**
       Summary: Return the number of parameters of the given declaration.

       - {b See also}: {!get_arity}
*)
val get_domain_size : context -> func_decl -> int
	

(**
       Summary: Alias for [get_domain_size].

       - {b See also}: {!get_domain_size}
*)
val get_arity : context -> func_decl -> int
	

(**
       Summary: \[ [ get_domain c d i ] \]
       Return the sort of the i-th parameter of the given function declaration.
       
       - {b Precondition}: i < get_domain_size d

       - {b See also}: {!get_domain_size}
*)
val get_domain : context -> func_decl -> int -> sort
	


(**
  Summary: \[ [ get_domains c d ] \] is the array of parameters of [d].

  - {b See also}: {!get_domain_size}
  - {b See also}: {!get_domain}
*)
val get_domains: context -> func_decl -> sort array

(**
       Summary: \[ [ get_range c d ] \]
       Return the range of the given declaration. 

       If [d] is a constant (i.e., has zero arguments), then this
       function returns the sort of the constant.
*)
val get_range : context -> func_decl -> sort
	

(**
       Summary: Return the number of parameters associated with a declaration.
*)
val get_decl_num_parameters : context -> func_decl -> int
	

(**
       Summary: Return the parameter type associated with a declaration.
       
       @param c the context
       @param d the function declaration
       @param idx is the index of the named parameter it should be between 0 and the number of parameters.
*)
val get_decl_parameter_kind : context -> func_decl -> int -> parameter_kind
	

(**
       Summary: Return the integer value associated with an integer parameter.

       - {b Precondition}: get_decl_parameter_kind c d idx == PARAMETER_INT
*)
val get_decl_int_parameter : context -> func_decl -> int -> int
	

(**
       Summary: Return the double value associated with an double parameter.

       - {b Precondition}: get_decl_parameter_kind c d idx == PARAMETER_DOUBLE
*)
val get_decl_double_parameter : context -> func_decl -> int -> float
	

(**
       Summary: Return the double value associated with an double parameter.

       - {b Precondition}: get_decl_parameter_kind c d idx == PARAMETER_SYMBOL
*)
val get_decl_symbol_parameter : context -> func_decl -> int -> symbol
	

(**
       Summary: Return the sort value associated with a sort parameter.

       - {b Precondition}: get_decl_parameter_kind c d idx == PARAMETER_SORT
*)
val get_decl_sort_parameter : context -> func_decl -> int -> sort
	

(**
       Summary: Return the expresson value associated with an expression parameter.

       - {b Precondition}: get_decl_parameter_kind c d idx == PARAMETER_AST
*)
val get_decl_ast_parameter : context -> func_decl -> int -> ast
	

(**
       Summary: Return the expresson value associated with an expression parameter.

       - {b Precondition}: get_decl_parameter_kind c d idx == PARAMETER_FUNC_DECL
*)
val get_decl_func_decl_parameter : context -> func_decl -> int -> func_decl
	

(**
       Summary: Return the rational value, as a string, associated with a rational parameter.

       - {b Precondition}: get_decl_parameter_kind c d idx == PARAMETER_RATIONAL
*)
val get_decl_rational_parameter : context -> func_decl -> int -> string
	

(**
        {3 {L Applications}} 
*)
(**
       Summary: Convert a [app] into [ast]. 
        - {b Remarks}: [app_to_ast c a] can be replaced by [(a :> ast)]. 
*)
val app_to_ast : context -> app -> ast
	

(**
       Summary: Return the declaration of a constant or function application.
*)
val get_app_decl : context -> app -> func_decl
	

(**
       Summary: \[ [ get_app_num_args c a ] \]
       Return the number of argument of an application. If [t]
       is an constant, then the number of arguments is 0.
*)
val get_app_num_args : context -> app -> int
	

(**
       Summary: \[ [ get_app_arg c a i ] \]
       Return the i-th argument of the given application.
       
       - {b Precondition}: i < get_num_args c a
*)
val get_app_arg : context -> app -> int -> ast
	


(**
  Summary: \[ [ get_app_args c a ] \] is the array of arguments of an application. If [t] is a constant, then the array is empty.

  - {b See also}: {!get_app_num_args}
  - {b See also}: {!get_app_arg}
*)
val get_app_args: context -> app -> ast array

(**
        {3 {L Terms}} 
*)

(**
  Summary: \[ [ binder_type ] \] is a universal or existential quantifier.

  - {b See also}: {!term_refined}
*)
type binder_type = Forall | Exists

(**
  Summary: \[ [ term_refined ] \] is the refinement of a {!ast} .

  - {b See also}: {!term_refine}
*)
type term_refined =
  | Term_numeral    of numeral_refined
  | Term_app        of decl_kind * func_decl * ast array
  | Term_quantifier of binder_type * int * ast array array * (symbol * sort) array * ast
  | Term_var        of int * sort


(**
   Summary: \[ [ mk_term c tr ] \] constructs the term described by [tr].

   - {b Precondition}: [tr] is not of form
   - {b See also}: {!term_refine}
*)
(* val mk_term: context -> term_refined -> ast *)


(**
   Summary: \[ [ term_refine c a ] \] is the refined view of [a].
*)
val term_refine : context -> ast -> term_refined

(**
       Summary: compare terms.
        - {b Remarks}: [Pervasives.( = )] or [Pervasives.compare] can also be used. 
*)
val is_eq_ast : context -> ast -> ast -> bool
	

(**
        Summary: Return a unique identifier for [t].
         - {b Remarks}: Implicitly used by [Pervasives.compare] for values of type [ast], [app], [sort], [func_decl], and [pattern]. 
*)
val get_ast_id : context -> ast -> int
	

(**
       Summary: Return a hash code for the given AST.
        - {b Remarks}: Implicitly used by [Hashtbl.hash] for values of type [ast], [app], [sort], [func_decl], and [pattern]. 
*)
val get_ast_hash : context -> ast -> int
	

(**
       Summary: Return the sort of an AST node.
       
       The AST node must be a constant, application, numeral, bound variable, or quantifier.

*)
val get_sort : context -> ast -> sort
	

(**
       Summary: Return true if the given expression [t] is well sorted.
*)
val is_well_sorted : context -> ast -> bool
	

(**
       Summary: Return L_TRUE if [a] is true, L_FALSE if it is false, and L_UNDEF otherwise.
*)
val get_bool_value : context -> ast -> lbool
	

(**
       Summary: Return the kind of the given AST.
*)
val get_ast_kind : context -> ast -> ast_kind
	

val is_app : context -> ast -> bool
	

val is_numeral_ast : context -> ast -> bool
	

(**
       Summary: Return true if the give AST is a real algebraic number.
*)
val is_algebraic_number : context -> ast -> bool
	

(**
       Summary: Convert an [ast] into an [APP_AST]. 
       
       - {b Precondition}: {v  get_ast_kind c a == [APP_AST]  v}
*)
val to_app : context -> ast -> app
	

(**
       Summary: Convert an AST into a FUNC_DECL_AST. This is just type casting.
       
       - {b Precondition}: {v  get_ast_kind c a == FUNC_DECL_AST  v}
*)
val to_func_decl : context -> ast -> func_decl
	

(**
        {4 {L Numerals}} 
*)

(**
   Summary: \[ [ numeral_refine c a ] \] is the refined view of [a].

   - {b Precondition}: [get_ast_kind c a = NUMERAL_AST]
*)
val numeral_refine : context -> ast -> numeral_refined

(**
        {5 {L Low-level API}} 
*)
(**
       Summary: Return numeral value, as a string of a numeric constant term

       - {b Precondition}: get_ast_kind c a == NUMERAL_AST
*)
val get_numeral_string : context -> ast -> string
	

(**
       Summary: Return numeral as a string in decimal notation.
       The result has at most [precision] decimal places.

       - {b Precondition}: get_ast_kind c a == NUMERAL_AST || is_algebraic_number c a
*)
val get_numeral_decimal_string : context -> ast -> int -> string
	

(**
       Summary: Return the numerator (as a numeral AST) of a numeral AST of sort Real.

       - {b Precondition}: get_ast_kind c a == NUMERAL_AST
*)
val get_numerator : context -> ast -> ast
	

(**
       Summary: Return the denominator (as a numeral AST) of a numeral AST of sort Real.

       - {b Precondition}: get_ast_kind c a == NUMERAL_AST
*)
val get_denominator : context -> ast -> ast
	

(**
       Summary: Return numeral value, as a pair of 64 bit numbers if the representation fits.

       @param c logical context.
       @param a term.
       @param num numerator.
       @param den denominator.
       
       Return [TRUE] if the numeral value fits in 64 bit numerals, [FALSE] otherwise.

       - {b Precondition}: get_ast_kind a == NUMERAL_AST
*)
val get_numeral_small : context -> ast -> bool * int64 * int64
	

(**
       Summary: \[ [ get_numeral_int c v ] \]
       Similar to {!get_numeral_string}, but only succeeds if
       the value can fit in a machine int. Return TRUE if the call succeeded.

       - {b Precondition}: get_ast_kind c v == NUMERAL_AST
      
       - {b See also}: {!get_numeral_string}
*)
val get_numeral_int : context -> ast -> bool * int
	

(**
       Summary: \[ [ get_numeral_int64 c v ] \]
       Similar to {!get_numeral_string}, but only succeeds if
       the value can fit in a machine long long int. Return TRUE if the call succeeded.

       - {b Precondition}: get_ast_kind c v == NUMERAL_AST

       - {b See also}: {!get_numeral_string}
*)
val get_numeral_int64 : context -> ast -> bool * int64
	

(**
       Summary: \[ [ get_numeral_rational_int64 c x y] \]
       Similar to {!get_numeral_string}, but only succeeds if
       the value can fit as a rational number as machine long long int. Return TRUE if the call succeeded.

       - {b Precondition}: get_ast_kind c v == NUMERAL_AST

       - {b See also}: {!get_numeral_string}
*)
val get_numeral_rational_int64 : context -> ast -> bool * int64 * int64
	

(**
       Summary: Return a lower bound for the given real algebraic number. 
       The interval isolating the number is smaller than 1/10^precision.
       The result is a numeral AST of sort Real.

       - {b Precondition}: is_algebraic_number c a
*)
val get_algebraic_number_lower : context -> ast -> int -> ast
	

(**
       Summary: Return a upper bound for the given real algebraic number. 
       The interval isolating the number is smaller than 1/10^precision.
       The result is a numeral AST of sort Real.

       - {b Precondition}: is_algebraic_number c a
*)
val get_algebraic_number_upper : context -> ast -> int -> ast
	

(**
        {4 {L Patterns}} 
*)
(**
       Summary: Convert a pattern into ast. 
        - {b Remarks}: [pattern_to_ast c p]  can be replaced by [(p :> ast)]. 
*)
val pattern_to_ast : context -> pattern -> ast
	


(**
  Summary: \[ [ get_pattern_terms c p ] \] is the ast's in pattern.

  - {b See also}: {!get_pattern_num_terms}
  - {b See also}: {!get_pattern}
*)
val get_pattern_terms: context -> pattern -> ast array;;

(**
        Summary: Return number of terms in pattern.
*)
val get_pattern_num_terms : context -> pattern -> int
	

(**
       Summary: Return i'th ast in pattern.
*)
val get_pattern : context -> pattern -> int -> ast
	

(**
        {4 {L Quantifiers}} 
*)
(**
       Summary: Return index of de-Brujin bound variable.

       - {b Precondition}: get_ast_kind a == VAR_AST
*)
val get_index_value : context -> ast -> int
	

(**
       Summary: Determine if quantifier is universal.
       
       - {b Precondition}: get_ast_kind a == QUANTIFIER_AST
*)
val is_quantifier_forall : context -> ast -> bool
	

(**
       Summary: Obtain weight of quantifier.
       
       - {b Precondition}: get_ast_kind a == QUANTIFIER_AST
*)
val get_quantifier_weight : context -> ast -> int
	

(**
       Summary: Return number of patterns used in quantifier.
       
       - {b Precondition}: get_ast_kind a == QUANTIFIER_AST
*)
val get_quantifier_num_patterns : context -> ast -> int
	

(**
       Summary: Return i'th pattern.
       
       - {b Precondition}: get_ast_kind a == QUANTIFIER_AST
*)
val get_quantifier_pattern_ast : context -> ast -> int -> pattern
	

(**
       Summary: Return number of no_patterns used in quantifier.
       
       - {b Precondition}: get_ast_kind a == QUANTIFIER_AST
*)
val get_quantifier_num_no_patterns : context -> ast -> int
	

(**
       Summary: Return i'th no_pattern.
       
       - {b Precondition}: get_ast_kind a == QUANTIFIER_AST
*)
val get_quantifier_no_pattern_ast : context -> ast -> int -> ast
	

(**
       Summary: Return symbol of the i'th bound variable.
       
       - {b Precondition}: get_ast_kind a == QUANTIFIER_AST
*)
val get_quantifier_bound_name : context -> ast -> int -> symbol
	

(**
       Summary: Return sort of the i'th bound variable.
       
       - {b Precondition}: get_ast_kind a == QUANTIFIER_AST
*)
val get_quantifier_bound_sort : context -> ast -> int -> sort
	

(**
       Summary: Return body of quantifier.
       
       - {b Precondition}: get_ast_kind a == QUANTIFIER_AST
*)
val get_quantifier_body : context -> ast -> ast
	

(**
       Summary: Return number of bound variables of quantifier.
       
       - {b Precondition}: get_ast_kind a == QUANTIFIER_AST
*)
val get_quantifier_num_bound : context -> ast -> int
	

(**
        {3 {L Simplification}} 
*)
(**
        Summary: Interface to simplifier.

        Provides an interface to the AST simplifier used by Z3.
*)
val simplify : context -> ast -> ast
	

(**
        Summary: Interface to simplifier.
        
        Provides an interface to the AST simplifier used by Z3.
        This procedure is similar to {!simplify}, but the behavior of the simplifier 
        can be configured using the given parameter set.
*)
val simplify_ex : context -> ast -> params -> ast
	

(**
       Summary: Return a string describing all available parameters.
*)
val simplify_get_help : context -> string
	

(**
       {2 {L Modifiers}}
*)
(**
       Summary: Update the arguments of term [a] using the arguments [args].
       The number of arguments [num_args] should coincide 
       with the number of arguments to [a].
       If [a] is a quantifier, then num_args has to be 1.
*)
val update_term : context -> ast -> ast array -> ast
	

(**
       Summary: Substitute every occurrence of {e from[i] } in [a] with {e to[i] }, for [i] smaller than [num_exprs].
       The result is the new AST. The arrays [from] and [to] must have size [num_exprs].
       For every [i] smaller than [num_exprs], we must have that sort of {e from[i] } must be equal to sort of {e to[i] }.
*)
val substitute : context -> ast -> ast array -> ast array -> ast
	

(**
       Summary: Substitute the free variables in [a] with the expressions in [to].
       For every [i] smaller than [num_exprs], the variable with de-Bruijn index [i] is replaced with term {e to[i] }.
*)
val substitute_vars : context -> ast -> ast array -> ast
	

(**
       Summary: Translate/Copy the AST [a] from context [source] to context [target].
       AST [a] must have been created using context [source].
       - {b Precondition}: source != target
*)
val translate : context -> ast -> context -> ast
	

(**
       {2 {L Models}}
*)

(**
   A model assigns uninterpreted sorts to finite universes of distinct values, constants to values,
   and arrays and functions to finite maps from argument values to result values plus a default
   value for all other arguments.
*)
type model_refined = {
  sorts : (sort, ast_vector) Hashtbl.t;
  consts : (func_decl, ast) Hashtbl.t;
  arrays : (func_decl, (ast, ast) Hashtbl.t * ast) Hashtbl.t;
  funcs : (func_decl, (ast array, ast) Hashtbl.t * ast) Hashtbl.t;
}


(** 
   Summary: [model_refine c m] is the refined model of [m].
*)
val model_refine : context -> model -> model_refined

(**
       Summary: \[ [ model_eval c m t ] \]
       Evaluate the AST node [t] in the given model. 
       
        Return [None] if the term was not successfully evaluated. 
       
       If [model_completion] is TRUE, then Z3 will assign an interpretation for any constant or function that does
       not have an interpretation in [m]. These constants and functions were essentially don't cares.

       The evaluation may fail for the following reasons:
       
       - [t] contains a quantifier.
       
       - the model [m] is partial, that is, it doesn't have a complete interpretation for uninterpreted functions. 
       That is, the option {e MODEL_PARTIAL=true } was used.
       
       - [t] is type incorrect.
*)
val model_eval : context -> model -> ast -> bool -> ast option
	

(**
        {4 {L Low-level API}} 
*)
(**
       Summary: Return the interpretation (i.e., assignment) of constant [a] in the model [m].
       Return  [None],  
       if the model does not assign an interpretation for [a]. 
       That should be interpreted as: the value of [a] does not matter.

       - {b Precondition}: get_arity c a == 0
*)
val model_get_const_interp : context -> model -> func_decl -> ast option
	

(**
       Summary: Return the interpretation of the function [f] in the model [m].
       Return  [None],  
       if the model does not assign an interpretation for [f]. 
       That should be interpreted as: the [f] does not matter.
       
       - {b Precondition}: get_arity c f > 0

       
       
*)
val model_get_func_interp : context -> model -> func_decl -> func_interp option
	

(**
       Summary: Return the number of constants assigned by the given model.
       
       - {b See also}: {!model_get_const_decl}
*)
val model_get_num_consts : context -> model -> int
	

(**
       Summary: \[ [ model_get_const_decl c m i ] \]
       Return the i-th constant in the given model. 

       - {b Precondition}: i < model_get_num_consts c m

       - {b See also}: {!model_eval}
*)
val model_get_const_decl : context -> model -> int -> func_decl
	

(**
       Summary: Return the number of function interpretations in the given model.
       
       A function interpretation is represented as a finite map and an 'else' value.
       Each entry in the finite map represents the value of a function given a set of arguments.
*)
val model_get_num_funcs : context -> model -> int
	

(**
       Summary: \[ [ model_get_func_decl c m i ] \]
       Return the declaration of the i-th function in the given model.

       - {b Precondition}: i < model_get_num_funcs c m

       - {b See also}: {!model_get_num_funcs}
*)
val model_get_func_decl : context -> model -> int -> func_decl
	

(**
       Summary: Return the number of uninterpreted sorts that [m] assigs an interpretation to.
       
       Z3 also provides an intepretation for uninterpreted sorts used in a formua.
       The interpretation for a sort [s] is a finite set of distinct values. We say this finite set is
       the "universe" of [s].
       
       - {b See also}: {!model_get_sort}
       - {b See also}: {!model_get_sort_universe}
*)
val model_get_num_sorts : context -> model -> int
	

(**
       Summary: Return a uninterpreted sort that [m] assigns an interpretation.
       
       - {b Precondition}: i < model_get_num_sorts c m
    
       - {b See also}: {!model_get_num_sorts}
       - {b See also}: {!model_get_sort_universe}
*)
val model_get_sort : context -> model -> int -> sort
	

(**
       Summary: Return the finite set of distinct values that represent the interpretation for sort [s].
       
       - {b See also}: {!model_get_num_sorts}
       - {b See also}: {!model_get_sort}
*)
val model_get_sort_universe : context -> model -> sort -> ast_vector
	

(**
       Summary: The {e (_ as-array f) } AST node is a construct for assigning interpretations for arrays in Z3.
       It is the array such that forall indices [i] we have that {e (select (_ as-array f) i) } is equal to {e (f i) }.
       This procedure returns TRUE if the [a] is an [as-array] AST node.

       Z3 current solvers have minimal support for [as_array] nodes. 

       - {b See also}: {!get_as_array_func_decl}
*)
val is_as_array : context -> ast -> bool
	

(**
       Summary: Return the function declaration [f] associated with a {e (_ as_array f) } node.
    
       - {b See also}: {!is_as_array}
*)
val get_as_array_func_decl : context -> ast -> func_decl
	

(**
       Summary: Return the number of entries in the given function interpretation.

       A function interpretation is represented as a finite map and an 'else' value.
       Each entry in the finite map represents the value of a function given a set of arguments.
       This procedure return the number of element in the finite map of [f].
*)
val func_interp_get_num_entries : context -> func_interp -> int
	

(**
       Summary: Return a "point" of the given function intepretation. It represents the
       value of [f] in a particular point.

       - {b Precondition}: i < func_interp_get_num_entries c f
       
       - {b See also}: {!func_interp_get_num_entries}
*)
val func_interp_get_entry : context -> func_interp -> int -> func_entry
	

(**
       Summary: Return the 'else' value of the given function interpretation.

       A function interpretation is represented as a finite map and an 'else' value.
       This procedure returns the 'else' value.
*)
val func_interp_get_else : context -> func_interp -> ast
	

(**
       Summary: Return the arity (number of arguments) of the given function interpretation.
*)
val func_interp_get_arity : context -> func_interp -> int
	

(**
       Summary: Return the value of this point. 
       
       A func_entry object represents an element in the finite map used to encode
       a function interpretation.
       
       - {b See also}: {!func_interp_get_entry}
*)
val func_entry_get_value : context -> func_entry -> ast
	

(**
       Summary: Return the number of arguments in a func_entry object.
       
       - {b See also}: {!func_interp_get_entry}
*)
val func_entry_get_num_args : context -> func_entry -> int
	

(**
       Summary: Return an argument of a func_entry object.

       - {b Precondition}: i < func_entry_get_num_args c e

       - {b See also}: {!func_interp_get_entry}
*)
val func_entry_get_arg : context -> func_entry -> int -> ast
	

(**
       {2 {L Interaction logging.}}
*)
(**
       Summary: Log interaction to a file.
*)
val open_log : string -> bool
	

(**
       Summary: Append user-defined string to interaction log.
       
       The interaction log is opened using open_log.
       It contains the formulas that are checked using Z3.
       You can use this command to append comments, for instance.
*)
val append_log : string -> unit
	

(**
       Summary: Close interaction log.
*)
val close_log : unit -> unit
	

(**
       Summary: Enable/disable printing warning messages to the console.

       Warnings are printed after passing [true], warning messages are
       suppressed after calling this method with [false].       
*)
val toggle_warning_messages : bool -> unit
	

(**
       {2 {L String conversion}}
*)
(**
       Summary: Select mode for the format used for pretty-printing AST nodes.

       The default mode for pretty printing AST nodes is to produce
       SMT-LIB style output where common subexpressions are printed 
       at each occurrence. The mode is called PRINT_SMTLIB_FULL.
       To print shared common subexpressions only once, 
       use the PRINT_LOW_LEVEL mode.
       To print in way that conforms to SMT-LIB standards and uses let
       expressions to share common sub-expressions use PRINT_SMTLIB_COMPLIANT.

       - {b See also}: {!ast_to_string}
       - {b See also}: {!pattern_to_string}
       - {b See also}: {!func_decl_to_string}

*)
val set_ast_print_mode : context -> ast_print_mode -> unit
	

(**
       Summary: Convert the given AST node into a string.

       
       
       
       - {b See also}: {!pattern_to_string}
       - {b See also}: {!sort_to_string}
*)
val ast_to_string : context -> ast -> string
	

val pattern_to_string : context -> pattern -> string
	

val sort_to_string : context -> sort -> string
	

val func_decl_to_string : context -> func_decl -> string
	

(**
       Summary: Convert the given model into a string.

       
       
       
*)
val model_to_string : context -> model -> string
	

(**
       Summary: Convert the given benchmark into SMT-LIB formatted string.

       
       
       

       @param c - context.
       @param name - name of benchmark. The argument is optional.
       @param logic - the benchmark logic. 
       @param status - the status string (sat, unsat, or unknown)
       @param attributes - other attributes, such as source, difficulty or category.
       @param num_assumptions - number of assumptions.
       @param assumptions - auxiliary assumptions.
       @param formula - formula to be checked for consistency in conjunction with assumptions.
*)
val benchmark_to_smtlib_string : context -> string -> string -> string -> string -> ast array -> ast -> string
	

(**
       {2 {L Parser interface}}
*)
(**
       Summary: \[ [ parse_smtlib2_string c str ] \]
       Parse the given string using the SMT-LIB2 parser. 
              
       It returns a formula comprising of the conjunction of assertions in the scope
       (up to push/pop) at the end of the string.
*)
val parse_smtlib2_string : context -> string -> symbol array -> sort array -> symbol array -> func_decl array -> ast
	

(**
       Summary: Similar to {!parse_smtlib2_string}, but reads the benchmark from a file.
*)
val parse_smtlib2_file : context -> string -> symbol array -> sort array -> symbol array -> func_decl array -> ast
	


(**
  Summary: \[ [ parse_smtlib_string_x c str sort_names sorts decl_names decls ] \]

  Parse the given string using the SMT-LIB parser. 

  The symbol table of the parser can be initialized using the given sorts and declarations. 
  The symbols in the arrays [sort_names] and [decl_names] don't need to match the names
  of the sorts and declarations in the arrays [sorts] and [decls]. This is an useful feature
  since we can use arbitrary names to reference sorts and declarations defined using the API.

  - {b See also}: {!parse_smtlib_file_x}
*)
val parse_smtlib_string_x: context -> string -> symbol array -> sort array -> symbol array -> func_decl array -> (ast array * ast array * func_decl array)

(**
  Summary: Similar to {!parse_smtlib_string_x}, but reads the benchmark from a file.

  - {b See also}: {!parse_smtlib_string_x}
*)
val parse_smtlib_file_x: context -> string -> symbol array -> sort array -> symbol array -> func_decl array -> (ast array * ast array * func_decl array)

(**
  Summary: \[ [ parse_smtlib_string_formula c ... ] \] calls [(parse_smtlib_string c ...)] and returns the single formula produced.

  - {b See also}: {!parse_smtlib_file_formula}
  - {b See also}: {!parse_smtlib_string_x}
*)
val parse_smtlib_string_formula: context -> string -> symbol array -> sort array -> symbol array -> func_decl array -> ast

(**
  Summary: \[ [ parse_smtlib_file_formula c ... ] \] calls [(parse_smtlib_file c ...)] and returns the single formula produced.

  - {b See also}: {!parse_smtlib_string_formula}
  - {b See also}: {!parse_smtlib_file_x}
*)
val parse_smtlib_file_formula: context -> string -> symbol array -> sort array -> symbol array -> func_decl array -> ast

(**
        {4 {L Low-level API}} 
*)
(**
       Summary: \[ [ parse_smtlib_string c str sort_names sorts decl_names decls ] \]
       Parse the given string using the SMT-LIB parser. 
              
       The symbol table of the parser can be initialized using the given sorts and declarations. 
       The symbols in the arrays [sort_names] and [decl_names] don't need to match the names
       of the sorts and declarations in the arrays [sorts] and [decls]. This is an useful feature
       since we can use arbitrary names to reference sorts and declarations defined using the C API.

       The formulas, assumptions and declarations defined in [str] can be extracted using the functions:
       {!get_smtlib_num_formulas}, {!get_smtlib_formula}, {!get_smtlib_num_assumptions}, {!get_smtlib_assumption}, 
       {!get_smtlib_num_decls}, and {!get_smtlib_decl}.
*)
val parse_smtlib_string : context -> string -> symbol array -> sort array -> symbol array -> func_decl array -> unit
	

(**
       Summary: Similar to {!parse_smtlib_string}, but reads the benchmark from a file.
*)
val parse_smtlib_file : context -> string -> symbol array -> sort array -> symbol array -> func_decl array -> unit
	

(**
       Summary: Return the number of SMTLIB formulas parsed by the last call to {!parse_smtlib_string} or {!parse_smtlib_file}.
*)
val get_smtlib_num_formulas : context -> int
	

(**
       Summary: \[ [ get_smtlib_formula c i ] \]
       Return the i-th formula parsed by the last call to {!parse_smtlib_string} or {!parse_smtlib_file}.

       - {b Precondition}: i < get_smtlib_num_formulas c
*)
val get_smtlib_formula : context -> int -> ast
	

(**
       Summary: Return the number of SMTLIB assumptions parsed by {!parse_smtlib_string} or {!parse_smtlib_file}.
*)
val get_smtlib_num_assumptions : context -> int
	

(**
       Summary: \[ [ get_smtlib_assumption c i ] \]
       Return the i-th assumption parsed by the last call to {!parse_smtlib_string} or {!parse_smtlib_file}.

       - {b Precondition}: i < get_smtlib_num_assumptions c
*)
val get_smtlib_assumption : context -> int -> ast
	

(**
       Summary: Return the number of declarations parsed by {!parse_smtlib_string} or {!parse_smtlib_file}.
*)
val get_smtlib_num_decls : context -> int
	

(**
       Summary: \[ [ get_smtlib_decl c i ] \]
       Return the i-th declaration parsed by the last call to {!parse_smtlib_string} or {!parse_smtlib_file}.

       - {b Precondition}: i < get_smtlib_num_decls c
*)
val get_smtlib_decl : context -> int -> func_decl
	

(**
       Summary: Return the number of sorts parsed by {!parse_smtlib_string} or {!parse_smtlib_file}.
*)
val get_smtlib_num_sorts : context -> int
	

(**
       Summary: \[ [ get_smtlib_sort c i ] \]
       Return the i-th sort parsed by the last call to {!parse_smtlib_string} or {!parse_smtlib_file}.

       - {b Precondition}: i < get_smtlib_num_sorts c
*)
val get_smtlib_sort : context -> int -> sort
	

(*
(**
       Summary: \[ [ get_smtlib_error c ] \]
       Retrieve that last error message information generated from parsing.
*)
val get_smtlib_error : context -> string
	

*)
(**
       Summary: \[ [ parse_z3_string c str ] \]
       Parse the given string using the Z3 native parser.
       
       Return the conjunction of asserts made in the input.
*)
val parse_z3_string : context -> string -> ast
	

(**
       Summary: Similar to {!parse_z3_string}, but reads the benchmark from a file.
*)
val parse_z3_file : context -> string -> ast
	

(**
       {2 {L Error Handling}}
*)
(**
       Summary: Set an error.
*)
val set_error : context -> error_code -> unit
	

(*
(**
       Summary: Return a string describing the given error code.
*)
val get_error_msg_ex : context -> error_code -> string
	

*)

(**
   Summary: Return a string describing the given error code.
*) 
val get_error_msg: context -> error_code -> string

(**
       {2 {L Miscellaneous}}
*)
(**
       Summary: Return Z3 version number information.
*)
val get_version : unit -> int * int * int * int
	

(**
        {2 {L Fixedpoint facilities}}
*)
(**
       Summary: Create a new fixedpoint context. 
       
       
       
*)
val mk_fixedpoint : context -> fixedpoint
	

(**
       Summary: Add a universal Horn clause as a named rule.
       The [horn_rule] should be of the form:
 
       {v 
           horn_rule ::= (forall (bound-vars) horn_rule)
                      |  (=> atoms horn_rule)
                      |  atom
        v}
*)
val fixedpoint_add_rule : context -> fixedpoint -> ast -> symbol -> unit
	

(**
       Summary: Add a Database fact. 
             
       @param c - context
       @param d - fixed point context
       @param r - relation signature for the row.
       @param num_args - number of columns for the given row. 
       @param args - array of the row elements.

       The number of arguments [num_args] should be equal to the number 
       of sorts in the domain of [r]. Each sort in the domain should be an integral
      (bit-vector, Boolean or or finite domain sort).

       The call has the same effect as adding a rule where  is applied to the arguments.
 
*)
val fixedpoint_add_fact : context -> fixedpoint -> func_decl -> int array -> unit
	

(**
       Summary: Assert a constraint to the fixedpoint context.

       The constraints are used as background axioms when the fixedpoint engine uses the PDR mode.
       They are ignored for standard Datalog mode.
*)
val fixedpoint_assert : context -> fixedpoint -> ast -> unit
	

(**
        Summary: Pose a query against the asserted rules.

        {v 
           query ::= (exists (bound-vars) query)
                 |  literals 
         v}

        query returns 
        - L_FALSE if the query is unsatisfiable.
        - L_TRUE if the query is satisfiable. Obtain the answer by calling {!fixedpoint_get_answer}.
        - L_UNDEF if the query was interrupted, timed out or otherwise failed.

*)
val fixedpoint_query : context -> fixedpoint -> ast -> lbool
	

(**
        Summary: Pose multiple queries against the asserted rules.

        The queries are encoded as relations (function declarations).
         
        query returns 
        - L_FALSE if the query is unsatisfiable.
        - L_TRUE if the query is satisfiable. Obtain the answer by calling {!fixedpoint_get_answer}.
        - L_UNDEF if the query was interrupted, timed out or otherwise failed.

*)
val fixedpoint_query_relations : context -> fixedpoint -> func_decl array -> lbool
	

(**
       Summary: Retrieve a formula that encodes satisfying answers to the query.

       
       When used in Datalog mode, the returned answer is a disjunction of conjuncts.
       Each conjunct encodes values of the bound variables of the query that are satisfied.
       In PDR mode, the returned answer is a single conjunction.

       The previous call to fixedpoint_query must have returned L_TRUE.
*)
val fixedpoint_get_answer : context -> fixedpoint -> ast
	

(**
       Summary: Retrieve a string that describes the last status returned by {!fixedpoint_query}.

       Use this method when {!fixedpoint_query} returns L_UNDEF.
*)
val fixedpoint_get_reason_unknown : context -> fixedpoint -> string
	

(**
       Summary: Retrieve statistics information from the last call to {!fixedpoint_query}.
*)
val fixedpoint_get_statistics : context -> fixedpoint -> stats
	

(**
       Summary: Register relation as Fixedpoint defined.
       Fixedpoint defined relations have least-fixedpoint semantics.
       For example, the relation is empty if it does not occur
       in a head or a fact.
*)
val fixedpoint_register_relation : context -> fixedpoint -> func_decl -> unit
	

(**
       Summary: Configure the predicate representation.

       It sets the predicate to use a set of domains given by the list of symbols.
       The domains given by the list of symbols must belong to a set
       of built-in domains.
*)
val fixedpoint_set_predicate_representation : context -> fixedpoint -> func_decl -> symbol array -> unit
	

(**
        Summary: Simplify rules into a set of new rules that are returned.
        The simplification routines apply inlining, quantifier elimination, and other
        algebraic simplifications.
*)
val fixedpoint_simplify_rules : context -> fixedpoint -> ast array -> func_decl array -> ast_vector
	

(**
       Summary: Print the current rules and background axioms as a string.
       @param c - context.
       @param f - fixedpoint context.
       @param num_queries - number of additional queries to print.
       @param queries - additional queries.
*)
val fixedpoint_to_string : context -> fixedpoint -> ast array -> string
	

(**
        {2 {L AST vectors}}
*)
(**
       Summary: Return an empty AST vector.

       
       
*)
val mk_ast_vector : context -> ast_vector
	

(**
       Summary: Return the size of the given AST vector.
*)
val ast_vector_size : context -> ast_vector -> int
	

(**
       Summary: Return the AST at position [i] in the AST vector [v].
       
       - {b Precondition}: i < ast_vector_size c v
*)
val ast_vector_get : context -> ast_vector -> int -> ast
	

(**
       Summary: Update position [i] of the AST vector [v] with the AST [a]. 

       - {b Precondition}: i < ast_vector_size c v
*)
val ast_vector_set : context -> ast_vector -> int -> ast -> unit
	

(**
       Summary: Resize the AST vector [v]. 
*)
val ast_vector_resize : context -> ast_vector -> int -> unit
	

(**
       Summary: Add the AST [a] in the end of the AST vector [v]. The size of [v] is increased by one.
*)
val ast_vector_push : context -> ast_vector -> ast -> unit
	

(**
       Summary: Translate the AST vector [v] from context [s] into an AST vector in context [t].
*)
val ast_vector_translate : context -> ast_vector -> context -> ast_vector
	

(**
       Summary: Convert AST vector into a string.
*)
val ast_vector_to_string : context -> ast_vector -> string
	

(**
        {2 {L AST maps}}
*)
(**
       Summary: Return an empty mapping from AST to AST

       
       
*)
val mk_ast_map : context -> ast_map
	

(**
       Summary: Return true if the map [m] contains the AST key [k].
*)
val ast_map_contains : context -> ast_map -> ast -> bool
	

(**
       Summary: Return the value associated with the key [k].
       
       The procedure invokes the error handler if [k] is not in the map.
*)
val ast_map_find : context -> ast_map -> ast -> ast
	

(**
       Summary: Store/Replace a new key, value pair in the given map.
*)
val ast_map_insert : context -> ast_map -> ast -> ast -> unit
	

(**
       Summary: Erase a key from the map.
*)
val ast_map_erase : context -> ast_map -> ast -> unit
	

(**
       Summary: Remove all keys from the given map.
*)
val ast_map_reset : context -> ast_map -> unit
	

(**
       Summary: Return the size of the given map.
*)
val ast_map_size : context -> ast_map -> int
	

(**
       Summary: Return the keys stored in the given map.
*)
val ast_map_keys : context -> ast_map -> ast_vector
	

(**
       Summary: Convert the given map into a string.
*)
val ast_map_to_string : context -> ast_map -> string
	

(**
        {2 {L Goals}}
*)
(**
       Summary: Create a goal (aka problem). A goal is essentially a set
       of formulas, that can be solved and/or transformed using
       tactics and solvers.
       
       If models == true, then model generation is enabled for the new goal.

       If unsat_cores == true, then unsat core generation is enabled for the new goal.

       If proofs == true, then proof generation is enabled for the new goal. Remark, the 
       Z3 context c must have been created with proof generation support.

       
       
*)
val mk_goal : context -> bool -> bool -> bool -> goal
	

(**
       Summary: Return the "precision" of the given goal. Goals can be transformed using over and under approximations.
       A under approximation is applied when the objective is to find a model for a given goal.
       An over approximation is applied when the objective is to find a proof for a given goal.
*)
val goal_precision : context -> goal -> goal_prec
	

(**
       Summary: Add a new formula [a] to the given goal. 
*)
val goal_assert : context -> goal -> ast -> unit
	

(**
       Summary: Return true if the given goal contains the formula [false].
*)
val goal_inconsistent : context -> goal -> bool
	

(**
       Summary: Return the depth of the given goal. It tracks how many transformations were applied to it.
*)
val goal_depth : context -> goal -> int
	

(**
       Summary: Erase all formulas from the given goal.
*)
val goal_reset : context -> goal -> unit
	

(**
       Summary: Return the number of formulas in the given goal.
*)
val goal_size : context -> goal -> int
	

(**
       Summary: Return a formula from the given goal.

       - {b Precondition}: idx < goal_size c g
*)
val goal_formula : context -> goal -> int -> ast
	

(**
       Summary: Return the number of formulas, subformulas and terms in the given goal.
*)
val goal_num_exprs : context -> goal -> int
	

(**
       Summary: Return true if the goal is empty, and it is precise or the product of a under approximation.
*)
val goal_is_decided_sat : context -> goal -> bool
	

(**
       Summary: Return true if the goal contains false, and it is precise or the product of an over approximation.
*)
val goal_is_decided_unsat : context -> goal -> bool
	

(**
       Summary: Copy a goal [g] from the context [source] to a the context [target].
*)
val goal_translate : context -> goal -> context -> goal
	

(**
       Summary: Convert a goal into a string.
*)
val goal_to_string : context -> goal -> string
	

(**
        {2 {L Tactics and Probes}}
*)
(**
       Summary: Return a tactic associated with the given name.
       The complete list of tactics may be obtained using the procedures {!get_num_tactics} and {!get_tactic_name}.
       It may also be obtained using the command {e (help-tactics) } in the SMT 2.0 front-end.
    
       Tactics are the basic building block for creating custom solvers for specific problem domains.
*)
val mk_tactic : context -> string -> tactic
	

(**
       Summary: Return a probe associated with the given name.
       The complete list of probes may be obtained using the procedures {!get_num_probes} and {!get_probe_name}.
       It may also be obtained using the command {e (help-tactics) } in the SMT 2.0 front-end.

       Probes are used to inspect a goal (aka problem) and collect information that may be used to decide
       which solver and/or preprocessing step will be used.
*)
val mk_probe : context -> string -> probe
	

(**
       Summary: Return a tactic that applies [t1] to a given goal and [t2]
       to every subgoal produced by t1.
*)
val tactic_and_then : context -> tactic -> tactic -> tactic
	

(**
       Summary: Return a tactic that first applies [t1] to a given goal,
       if it fails then returns the result of [t2] applied to the given goal.
*)
val tactic_or_else : context -> tactic -> tactic -> tactic
	

(**
       Summary: Return a tactic that applies the given tactics in parallel.
*)
val tactic_par_or : context -> tactic array -> tactic
	

(**
       Summary: Return a tactic that applies [t1] to a given goal and then [t2]
       to every subgoal produced by t1. The subgoals are processed in parallel.
*)
val tactic_par_and_then : context -> tactic -> tactic -> tactic
	

(**
       Summary: Return a tactic that applies [t] to a given goal for [ms] milliseconds.
       If [t] does not terminate in [ms] milliseconds, then it fails.
*)
val tactic_try_for : context -> tactic -> int -> tactic
	

(**
       Summary: Return a tactic that applies [t] to a given goal is the probe [p] evaluates to true.
       If [p] evaluates to false, then the new tactic behaves like the skip tactic.
*)
val tactic_when : context -> probe -> tactic -> tactic
	

(**
       Summary: Return a tactic that applies [t1] to a given goal if the probe [p] evaluates to true,
       and [t2] if [p] evaluates to false.
*)
val tactic_cond : context -> probe -> tactic -> tactic -> tactic
	

(**
       Summary: Return a tactic that keeps applying [t] until the goal is not modified anymore or the maximum
       number of iterations [max] is reached.
*)
val tactic_repeat : context -> tactic -> int -> tactic
	

(**
       Summary: Return a tactic that just return the given goal.
*)
val tactic_skip : context -> tactic
	

(**
       Summary: Return a tactic that always fails.
*)
val tactic_fail : context -> tactic
	

(**
       Summary: Return a tactic that fails if the probe [p] evaluates to false.
*)
val tactic_fail_if : context -> probe -> tactic
	

(**
       Summary: Return a tactic that fails if the goal is not trivially satisfiable (i.e., empty) or
       trivially unsatisfiable (i.e., contains false).
*)
val tactic_fail_if_not_decided : context -> tactic
	

(**
       Summary: Return a tactic that applies [t] using the given set of parameters.
*)
val tactic_using_params : context -> tactic -> params -> tactic
	

(**
       Summary: Return a probe that always evaluates to val.
*)
val probe_const : context -> float -> probe
	

(**
       Summary: Return a probe that evaluates to "true" when the value returned by [p1] is less than the value returned by [p2].

       - {b Remarks}: For probes, "true" is any value different from 0.0.
*)
val probe_lt : context -> probe -> probe -> probe
	

(**
       Summary: Return a probe that evaluates to "true" when the value returned by [p1] is greater than the value returned by [p2].

       - {b Remarks}: For probes, "true" is any value different from 0.0.
*)
val probe_gt : context -> probe -> probe -> probe
	

(**
       Summary: Return a probe that evaluates to "true" when the value returned by [p1] is less than or equal to the value returned by [p2].

       - {b Remarks}: For probes, "true" is any value different from 0.0.
*)
val probe_le : context -> probe -> probe -> probe
	

(**
       Summary: Return a probe that evaluates to "true" when the value returned by [p1] is greater than or equal to the value returned by [p2].

       - {b Remarks}: For probes, "true" is any value different from 0.0.
*)
val probe_ge : context -> probe -> probe -> probe
	

(**
       Summary: Return a probe that evaluates to "true" when the value returned by [p1] is equal to the value returned by [p2].

       - {b Remarks}: For probes, "true" is any value different from 0.0.
*)
val probe_eq : context -> probe -> probe -> probe
	

(**
       Summary: Return a probe that evaluates to "true" when [p1] and [p2] evaluates to true.

       - {b Remarks}: For probes, "true" is any value different from 0.0.
*)
val probe_and : context -> probe -> probe -> probe
	

(**
       Summary: Return a probe that evaluates to "true" when [p1] or [p2] evaluates to true.

       - {b Remarks}: For probes, "true" is any value different from 0.0.
*)
val probe_or : context -> probe -> probe -> probe
	

(**
       Summary: Return a probe that evaluates to "true" when [p] does not evaluate to true.

       - {b Remarks}: For probes, "true" is any value different from 0.0.
*)
val probe_not : context -> probe -> probe
	

(**
       Summary: Return the number of builtin tactics available in Z3.
*)
val get_num_tactics : context -> int
	

(**
       Summary: Return the name of the idx tactic.

       - {b Precondition}: i < get_num_tactics c
*)
val get_tactic_name : context -> int -> string
	

(**
       Summary: Return the number of builtin probes available in Z3.
*)
val get_num_probes : context -> int
	

(**
       Summary: Return the name of the i probe.

       - {b Precondition}: i < get_num_probes c
*)
val get_probe_name : context -> int -> string
	

(**
       Summary: Return a string containing a description of parameters accepted by the given tactic.
*)
val tactic_get_help : context -> tactic -> string
	

(**
       Summary: Return a string containing a description of the tactic with the given name.
*)
val tactic_get_descr : context -> string -> string
	

(**
       Summary: Return a string containing a description of the probe with the given name.
*)
val probe_get_descr : context -> string -> string
	

(**
       Summary: Execute the probe over the goal. The probe always produce a double value.
       "Boolean" probes return 0.0 for false, and a value different from 0.0 for true.
*)
val probe_apply : context -> probe -> goal -> float
	

(**
       Summary: Apply tactic [t] to the goal [g].
*)
val tactic_apply : context -> tactic -> goal -> apply_result
	

(**
       Summary: Apply tactic [t] to the goal [g] using the parameter set [p].
*)
val tactic_apply_ex : context -> tactic -> goal -> params -> apply_result
	

(**
       Summary: Convert the [apply_result] object returned by {!tactic_apply} into a string.
*)
val apply_result_to_string : context -> apply_result -> string
	

(**
       Summary: Return the number of subgoals in the [apply_result] object returned by {!tactic_apply}.
*)
val apply_result_get_num_subgoals : context -> apply_result -> int
	

(**
       Summary: Return one of the subgoals in the [apply_result] object returned by {!tactic_apply}.
       
       - {b Precondition}: i < apply_result_get_num_subgoals c r
*)
val apply_result_get_subgoal : context -> apply_result -> int -> goal
	

(**
       Summary: Convert a model for the subgoal [apply_result_get_subgoal(c], r, i) into a model for the original goal [g].
       Where [g] is the goal used to create [r] using [tactic_apply(c], t, g).
*)
val apply_result_convert_model : context -> apply_result -> int -> model -> model
	

(**
        {2 {L Solvers}}
*)
(**
       Summary: Create a new (incremental) solver. This solver also uses a
       set of builtin tactics for handling the first check-sat command, and
       check-sat commands that take more than a given number of milliseconds to be solved. 
       
       
       
*)
val mk_solver : context -> solver
	

(**
       Summary: Create a new (incremental) solver.
*)
val mk_simple_solver : context -> solver
	

(**
       Summary: Create a new solver customized for the given logic.
       It behaves like {!mk_solver} if the logic is unknown or unsupported.
       
       
       
*)
val mk_solver_for_logic : context -> symbol -> solver
	

(**
       Summary: Create a new solver that is implemented using the given tactic.
       The solver supports the commands {!solver_push} and {!solver_pop}, but it
       will always solve each {!solver_check} from scratch.
*)
val mk_solver_from_tactic : context -> tactic -> solver
	

(**
       Summary: Return a string describing all solver available parameters.
*)
val solver_get_help : context -> solver -> string
	

(**
       Summary: Set the given solver using the given parameters.
*)
val solver_set_params : context -> solver -> params -> unit
	

(**
       Summary: Create a backtracking point.
       
       The solver contains a stack of assertions. 

       - {b See also}: {!solver_pop}
*)
val solver_push : context -> solver -> unit
	

(**
       Summary: Backtrack [n] backtracking points.
       
       - {b See also}: {!solver_push}

       - {b Precondition}: n <= solver_get_num_scopes c s
*)
val solver_pop : context -> solver -> int -> unit
	

(**
       Summary: Remove all assertions from the solver.
*)
val solver_reset : context -> solver -> unit
	

(**
       Summary: Return the number of backtracking points.
       
       - {b See also}: {!solver_push}
       - {b See also}: {!solver_pop}
*)
val solver_get_num_scopes : context -> solver -> int
	

(**
       Summary: Assert a constraint into the solver.
       
       The functions {!solver_check} and {!solver_check_assumptions} should be
       used to check whether the logical context is consistent or not.
*)
val solver_assert : context -> solver -> ast -> unit
	

(**
       Summary: Return the set of asserted formulas as a goal object.
*)
val solver_get_assertions : context -> solver -> ast_vector
	

(**
       Summary: Check whether the assertions in a given solver are consistent or not.

       The function {!solver_get_model} retrieves a model if the
       assertions are not unsatisfiable (i.e., the result is not \c
       L_FALSE) and model construction is enabled.

       The function {!solver_get_proof} retrieves a proof if proof
       generation was enabled when the context was created, and the 
       assertions are unsatisfiable (i.e., the result is [L_FALSE)].
*)
val solver_check : context -> solver -> lbool
	

(**
       Summary: Check whether the assertions in the given solver and
       optional assumptions are consistent or not.

       The function {!solver_get_unsat_core} retrieves the subset of the 
       assumptions used in the unsatisfiability proof produced by Z3.
      
       - {b See also}: {!solver_check}
*)
val solver_check_assumptions : context -> solver -> ast array -> lbool
	

(**
       Summary: Retrieve the model for the last {!solver_check} or {!solver_check_assumptions}

       The error handler is invoked if a model is not available because 
       the commands above were not invoked for the given solver, or if the result was [L_FALSE].
*)
val solver_get_model : context -> solver -> model
	

(**
       Summary: Retrieve the proof for the last {!solver_check} or {!solver_check_assumptions}

       The error handler is invoked if proof generation is not enabled,
       or if the commands above were not invoked for the given solver,
       or if the result was different from [L_FALSE].
*)
val solver_get_proof : context -> solver -> ast
	

(**
       Summary: Retrieve the unsat core for the last {!solver_check_assumptions}
       The unsat core is a subset of the assumptions [a].
*)
val solver_get_unsat_core : context -> solver -> ast_vector
	

(**
       Summary: Return a brief justification for an "unknown" result (i.e., L_UNDEF) for
       the commands {!solver_check} and {!solver_check_assumptions}
*)
val solver_get_reason_unknown : context -> solver -> string
	

(**
       Summary: Return statistics for the given solver.

       
*)
val solver_get_statistics : context -> solver -> stats
	

(**
       Summary: Convert a solver into a string.
*)
val solver_to_string : context -> solver -> string
	

(**
        {2 {L Statistics}}
*)

type stat_datum = Stat_int of int | Stat_float of float
type stats_refined = (string, stat_datum) Hashtbl.t


(** 
   Summary: [stats_refine c s] is the refined stats of [s].
*)
val stats_refine : context -> stats -> stats_refined

(**
       Summary: Convert a statistics into a string.
*)
val stats_to_string : context -> stats -> string
	

(**
        {4 {L Low-level API}} 
*)
(**
       Summary: Return the number of statistical data in [s].
*)
val stats_size : context -> stats -> int
	

(**
       Summary: Return the key (a string) for a particular statistical data.

       - {b Precondition}: idx < stats_size c s
*)
val stats_get_key : context -> stats -> int -> string
	

(**
       Summary: Return TRUE if the given statistical data is a unsigned int integer.
       
       - {b Precondition}: idx < stats_size c s
*)
val stats_is_uint : context -> stats -> int -> bool
	

(**
       Summary: Return TRUE if the given statistical data is a double.
       
       - {b Precondition}: idx < stats_size c s
*)
val stats_is_double : context -> stats -> int -> bool
	

(**
       Summary: Return the unsigned int value of the given statistical data.
       
       - {b Precondition}: idx < stats_size c s && stats_is_uint c s
*)
val stats_get_uint_value : context -> stats -> int -> int
	

(**
       Summary: Return the double value of the given statistical data.
       
       - {b Precondition}: idx < stats_size c s && stats_is_double c s
*)
val stats_get_double_value : context -> stats -> int -> float
	


(** 
   {2 {L Legacy V3 API}} 
*) 

module V3 : sig
(** 
   {2 {L Legacy V3 API}} 
*) 

(* File generated from z3.idl *)

type symbol
and literals
and theory
and config
and context
and sort
and func_decl
and ast
and app
and pattern
and model
and constructor
and constructor_list

and lbool =
  | L_FALSE
  | L_UNDEF
  | L_TRUE

and symbol_kind =
  | INT_SYMBOL
  | STRING_SYMBOL

and parameter_kind =
  | PARAMETER_INT
  | PARAMETER_DOUBLE
  | PARAMETER_RATIONAL
  | PARAMETER_SYMBOL
  | PARAMETER_SORT
  | PARAMETER_AST
  | PARAMETER_FUNC_DECL

and sort_kind =
  | UNINTERPRETED_SORT
  | BOOL_SORT
  | INT_SORT
  | REAL_SORT
  | BV_SORT
  | ARRAY_SORT
  | DATATYPE_SORT
  | RELATION_SORT
  | FINITE_DOMAIN_SORT
  | UNKNOWN_SORT

and ast_kind =
  | NUMERAL_AST
  | APP_AST
  | VAR_AST
  | QUANTIFIER_AST
  | SORT_AST
  | FUNC_DECL_AST
  | UNKNOWN_AST

and decl_kind =
  | OP_TRUE
  | OP_FALSE
  | OP_EQ
  | OP_DISTINCT
  | OP_ITE
  | OP_AND
  | OP_OR
  | OP_IFF
  | OP_XOR
  | OP_NOT
  | OP_IMPLIES
  | OP_OEQ
  | OP_ANUM
  | OP_AGNUM
  | OP_LE
  | OP_GE
  | OP_LT
  | OP_GT
  | OP_ADD
  | OP_SUB
  | OP_UMINUS
  | OP_MUL
  | OP_DIV
  | OP_IDIV
  | OP_REM
  | OP_MOD
  | OP_TO_REAL
  | OP_TO_INT
  | OP_IS_INT
  | OP_POWER
  | OP_STORE
  | OP_SELECT
  | OP_CONST_ARRAY
  | OP_ARRAY_MAP
  | OP_ARRAY_DEFAULT
  | OP_SET_UNION
  | OP_SET_INTERSECT
  | OP_SET_DIFFERENCE
  | OP_SET_COMPLEMENT
  | OP_SET_SUBSET
  | OP_AS_ARRAY
  | OP_BNUM
  | OP_BIT1
  | OP_BIT0
  | OP_BNEG
  | OP_BADD
  | OP_BSUB
  | OP_BMUL
  | OP_BSDIV
  | OP_BUDIV
  | OP_BSREM
  | OP_BUREM
  | OP_BSMOD
  | OP_BSDIV0
  | OP_BUDIV0
  | OP_BSREM0
  | OP_BUREM0
  | OP_BSMOD0
  | OP_ULEQ
  | OP_SLEQ
  | OP_UGEQ
  | OP_SGEQ
  | OP_ULT
  | OP_SLT
  | OP_UGT
  | OP_SGT
  | OP_BAND
  | OP_BOR
  | OP_BNOT
  | OP_BXOR
  | OP_BNAND
  | OP_BNOR
  | OP_BXNOR
  | OP_CONCAT
  | OP_SIGN_EXT
  | OP_ZERO_EXT
  | OP_EXTRACT
  | OP_REPEAT
  | OP_BREDOR
  | OP_BREDAND
  | OP_BCOMP
  | OP_BSHL
  | OP_BLSHR
  | OP_BASHR
  | OP_ROTATE_LEFT
  | OP_ROTATE_RIGHT
  | OP_EXT_ROTATE_LEFT
  | OP_EXT_ROTATE_RIGHT
  | OP_INT2BV
  | OP_BV2INT
  | OP_CARRY
  | OP_XOR3
  | OP_PR_UNDEF
  | OP_PR_TRUE
  | OP_PR_ASSERTED
  | OP_PR_GOAL
  | OP_PR_MODUS_PONENS
  | OP_PR_REFLEXIVITY
  | OP_PR_SYMMETRY
  | OP_PR_TRANSITIVITY
  | OP_PR_TRANSITIVITY_STAR
  | OP_PR_MONOTONICITY
  | OP_PR_QUANT_INTRO
  | OP_PR_DISTRIBUTIVITY
  | OP_PR_AND_ELIM
  | OP_PR_NOT_OR_ELIM
  | OP_PR_REWRITE
  | OP_PR_REWRITE_STAR
  | OP_PR_PULL_QUANT
  | OP_PR_PULL_QUANT_STAR
  | OP_PR_PUSH_QUANT
  | OP_PR_ELIM_UNUSED_VARS
  | OP_PR_DER
  | OP_PR_QUANT_INST
  | OP_PR_HYPOTHESIS
  | OP_PR_LEMMA
  | OP_PR_UNIT_RESOLUTION
  | OP_PR_IFF_TRUE
  | OP_PR_IFF_FALSE
  | OP_PR_COMMUTATIVITY
  | OP_PR_DEF_AXIOM
  | OP_PR_DEF_INTRO
  | OP_PR_APPLY_DEF
  | OP_PR_IFF_OEQ
  | OP_PR_NNF_POS
  | OP_PR_NNF_NEG
  | OP_PR_NNF_STAR
  | OP_PR_CNF_STAR
  | OP_PR_SKOLEMIZE
  | OP_PR_MODUS_PONENS_OEQ
  | OP_PR_TH_LEMMA
  | OP_RA_STORE
  | OP_RA_EMPTY
  | OP_RA_IS_EMPTY
  | OP_RA_JOIN
  | OP_RA_UNION
  | OP_RA_WIDEN
  | OP_RA_PROJECT
  | OP_RA_FILTER
  | OP_RA_NEGATION_FILTER
  | OP_RA_RENAME
  | OP_RA_COMPLEMENT
  | OP_RA_SELECT
  | OP_RA_CLONE
  | OP_FD_LT
  | OP_LABEL
  | OP_LABEL_LIT
  | OP_UNINTERPRETED

and search_failure =
  | NO_FAILURE
  | UNKNOWN
  | TIMEOUT
  | MEMOUT_WATERMARK
  | CANCELED
  | NUM_CONFLICTS
  | THEORY
  | QUANTIFIERS

and ast_print_mode =
  | PRINT_SMTLIB_FULL
  | PRINT_LOW_LEVEL
  | PRINT_SMTLIB_COMPLIANT
  | PRINT_SMTLIB2_COMPLIANT


(**
   

*)
(**
   {2 {L Types}}
   
   
    Most of the types in the API are abstract. 

   
   - [context]: manager of all other Z3 objects, global configuration options, etc.
   - [symbol]: Lisp-like symbol used to name types, constants, and functions.  A symbol can be created using string or integers.
   - [ast]: abstract syntax tree node. That is, the data-structure used in Z3 to represent terms, formulas and types.
   - [sort]: kind of AST used to represent types.
   - [func_decl]: kind of AST used to represent function symbols.
   - [app]: kind of AST used to represent function applications.
   - [pattern]: kind of AST used to represent pattern and multi-patterns used to guide quantifier instantiation.
   
   - [params]: parameter set used to configure many components such as: simplifiers, tactics, solvers, etc.
   - [model]: model for the constraints asserted into the logical context.
   - [func_interp]: interpretation of a function in a model.
   - [func_entry]: representation of the value of a [func_interp] at a particular point.
   - [fixedpoint]: context for the recursive predicate solver.
   - [ast_vector]: vector of [ast] objects.
   - [ast_map]: mapping from [ast] to [ast] objects.
   - [goal]: set of formulas that can be solved and/or transformed using tactics and solvers.
   - [tactic]: basic building block for creating custom solvers for specific problem domains.
   - [probe]: function/predicate used to inspect a goal and collect information that may be used to decide which solver and/or preprocessing step will be used.
   - [apply_result]: collection of subgoals resulting from applying of a tactic to a goal.
   - [solver]: (incremental) solver, possibly specialized by a particular tactic or logic.
   - [stats]: statistical data for a solver.
*)
(**
    {!lbool}  
   Lifted Boolean type: [false], [undefined], [true].
*)
(**
    {!symbol_kind}  
   The different kinds of symbol.
   In Z3, a symbol can be represented using integers and strings (See {!get_symbol_kind}).

   - {b See also}: {!mk_int_symbol}
   - {b See also}: {!mk_string_symbol}
*)
(**
    {!parameter_kind}  
   The different kinds of parameters that can be associated with function symbols.
   - {b See also}: {!get_decl_num_parameters}
   - {b See also}: {!get_decl_parameter_kind}

   - PARAMETER_INT is used for integer parameters.
   - PARAMETER_DOUBLE is used for double parameters.
   - PARAMETER_RATIONAL is used for parameters that are rational numbers.
   - PARAMETER_SYMBOL is used for parameters that are symbols.
   - PARAMETER_SORT is used for sort parameters.
   - PARAMETER_AST is used for expression parameters.
   - PARAMETER_FUNC_DECL is used for function declaration parameters.
*)
(**
    {!sort_kind}  
   The different kinds of Z3 types (See {!get_sort_kind}).
*)
(**
    {!ast_kind}  
   The different kinds of Z3 AST (abstract syntax trees). That is, terms, formulas and types.

   - APP_AST:            constant and applications
   - NUMERAL_AST:        numeral constants
   - VAR_AST:            bound variables
   - QUANTIFIER_AST:     quantifiers
   - SORT_AST:           sort
   - FUNC_DECL_AST:      function declaration
   - UNKNOWN_AST:        internal
*)
(**
    {!decl_kind}  
   The different kinds of interpreted function kinds.

   - OP_TRUE The constant true.

   - OP_FALSE The constant false.

   - OP_EQ The equality predicate.

   - OP_DISTINCT The n-ary distinct predicate (every argument is mutually distinct).

   - OP_ITE The ternary if-then-else term.

   - OP_AND n-ary conjunction.

   - OP_OR n-ary disjunction.

   - OP_IFF equivalence (binary).

   - OP_XOR Exclusive or.

   - OP_NOT Negation.

   - OP_IMPLIES Implication.

   - OP_OEQ Binary equivalence modulo namings. This binary predicate is used in proof terms.
        It captures equisatisfiability and equivalence modulo renamings.

   - OP_ANUM Arithmetic numeral.

   - OP_AGNUM Arithmetic algebraic numeral. Algebraic numbers are used to represent irrational numbers in Z3.

   - OP_LE <=.

   - OP_GE >=.

   - OP_LT <.

   - OP_GT >.

   - OP_ADD Addition - Binary.

   - OP_SUB Binary subtraction.

   - OP_UMINUS Unary minus.

   - OP_MUL Multiplication - Binary.

   - OP_DIV Division - Binary.

   - OP_IDIV Integer division - Binary.

   - OP_REM Remainder - Binary.

   - OP_MOD Modulus - Binary.

   - OP_TO_REAL Coercion of integer to real - Unary.

   - OP_TO_INT Coercion of real to integer - Unary.

   - OP_IS_INT Check if real is also an integer - Unary.

   - OP_POWER Power operator x^y.

   - OP_STORE Array store. It satisfies select(store(a,i,v),j) = if i = j then v else select(a,j).
        Array store takes at least 3 arguments. 

   - OP_SELECT Array select. 

   - OP_CONST_ARRAY The constant array. For example, select(const(v),i) = v holds for every v and i. The function is unary.

   - OP_ARRAY_DEFAULT Default value of arrays. For example default(const(v)) = v. The function is unary.

   - OP_ARRAY_MAP Array map operator.
         It satisfies map[f](a1,..,a_n)[i] = f(a1[i],...,a_n[i]) for every i.

   - OP_SET_UNION Set union between two Booelan arrays (two arrays whose range type is Boolean). The function is binary.

   - OP_SET_INTERSECT Set intersection between two Boolean arrays. The function is binary.

   - OP_SET_DIFFERENCE Set difference between two Boolean arrays. The function is binary.

   - OP_SET_COMPLEMENT Set complement of a Boolean array. The function is unary.

   - OP_SET_SUBSET Subset predicate between two Boolean arrays. The relation is binary.

   - OP_AS_ARRAY An array value that behaves as the function graph of the
                    function passed as parameter.

   - OP_BNUM Bit-vector numeral.

   - OP_BIT1 One bit bit-vector.

   - OP_BIT0 Zero bit bit-vector.

   - OP_BNEG Unary minus.

   - OP_BADD Binary addition.

   - OP_BSUB Binary subtraction.

   - OP_BMUL Binary multiplication.
    
   - OP_BSDIV Binary signed division.

   - OP_BUDIV Binary unsigned int division.

   - OP_BSREM Binary signed remainder.

   - OP_BUREM Binary unsigned int remainder.

   - OP_BSMOD Binary signed modulus.

   - OP_BSDIV0 Unary function. bsdiv(x,0) is congruent to bsdiv0(x).

   - OP_BUDIV0 Unary function. budiv(x,0) is congruent to budiv0(x).

   - OP_BSREM0 Unary function. bsrem(x,0) is congruent to bsrem0(x).

   - OP_BUREM0 Unary function. burem(x,0) is congruent to burem0(x).

   - OP_BSMOD0 Unary function. bsmod(x,0) is congruent to bsmod0(x).
    
   - OP_ULEQ Unsigned bit-vector <= - Binary relation.

   - OP_SLEQ Signed bit-vector  <= - Binary relation.

   - OP_UGEQ Unsigned bit-vector  >= - Binary relation.

   - OP_SGEQ Signed bit-vector  >= - Binary relation.

   - OP_ULT Unsigned bit-vector  < - Binary relation.

   - OP_SLT Signed bit-vector < - Binary relation.

   - OP_UGT Unsigned bit-vector > - Binary relation.

   - OP_SGT Signed bit-vector > - Binary relation.

   - OP_BAND Bit-wise and - Binary.

   - OP_BOR Bit-wise or - Binary.

   - OP_BNOT Bit-wise not - Unary.

   - OP_BXOR Bit-wise xor - Binary.

   - OP_BNAND Bit-wise nand - Binary.

   - OP_BNOR Bit-wise nor - Binary.

   - OP_BXNOR Bit-wise xnor - Binary.

   - OP_CONCAT Bit-vector concatenation - Binary.

   - OP_SIGN_EXT Bit-vector sign extension.

   - OP_ZERO_EXT Bit-vector zero extension.

   - OP_EXTRACT Bit-vector extraction.

   - OP_REPEAT Repeat bit-vector n times.

   - OP_BREDOR Bit-vector reduce or - Unary.

   - OP_BREDAND Bit-vector reduce and - Unary.

   - OP_BCOMP .

   - OP_BSHL Shift left.

   - OP_BLSHR Logical shift right.

   - OP_BASHR Arithmetical shift right.

   - OP_ROTATE_LEFT Left rotation.

   - OP_ROTATE_RIGHT Right rotation.

   - OP_EXT_ROTATE_LEFT (extended) Left rotation. Similar to OP_ROTATE_LEFT, but it is a binary operator instead of a parametric one.

   - OP_EXT_ROTATE_RIGHT (extended) Right rotation. Similar to OP_ROTATE_RIGHT, but it is a binary operator instead of a parametric one.

   - OP_INT2BV Coerce integer to bit-vector. NB. This function
       is not supported by the decision procedures. Only the most
       rudimentary simplification rules are applied to this function.

   - OP_BV2INT Coerce bit-vector to integer. NB. This function
       is not supported by the decision procedures. Only the most
       rudimentary simplification rules are applied to this function.

   - OP_CARRY Compute the carry bit in a full-adder. 
       The meaning is given by the equivalence
       (carry l1 l2 l3) <=> (or (and l1 l2) (and l1 l3) (and l2 l3)))

   - OP_XOR3 Compute ternary XOR.
       The meaning is given by the equivalence
       (xor3 l1 l2 l3) <=> (xor (xor l1 l2) l3)

   - OP_PR_UNDEF: Undef/Null proof object.

   - OP_PR_TRUE: Proof for the expression 'true'.

   - OP_PR_ASSERTED: Proof for a fact asserted by the user.
   
   - OP_PR_GOAL: Proof for a fact (tagged as goal) asserted by the user.

   - OP_PR_MODUS_PONENS: Given a proof for p and a proof for (implies p q), produces a proof for q.
       {e
          T1: p
          T2: (implies p q)
          [mp T1 T2]: q
          }
          The second antecedents may also be a proof for (iff p q).

   - OP_PR_REFLEXIVITY: A proof for (R t t), where R is a reflexive relation. This proof object has no antecedents.
        The only reflexive relations that are used are 
        equivalence modulo namings, equality and equivalence.
        That is, R is either '~', '=' or 'iff'.

   - OP_PR_SYMMETRY: Given an symmetric relation R and a proof for (R t s), produces a proof for (R s t).
          {e
          T1: (R t s)
          [symmetry T1]: (R s t)
          }
          T1 is the antecedent of this proof object.

   - OP_PR_TRANSITIVITY: Given a transitive relation R, and proofs for (R t s) and (R s u), produces a proof
       for (R t u).
       {e
       T1: (R t s)
       T2: (R s u)
       [trans T1 T2]: (R t u)
       }

   - OP_PR_TRANSITIVITY_STAR: Condensed transitivity proof. This proof object is only used if the parameter PROOF_MODE is 1.
     It combines several symmetry and transitivity proofs. 

          Example:
          {e
          T1: (R a b)
          T2: (R c b)
          T3: (R c d)
          [trans* T1 T2 T3]: (R a d)
          }
          R must be a symmetric and transitive relation.

          Assuming that this proof object is a proof for (R s t), then
          a proof checker must check if it is possible to prove (R s t)
          using the antecedents, symmetry and transitivity.  That is, 
          if there is a path from s to t, if we view every
          antecedent (R a b) as an edge between a and b.

   - OP_PR_MONOTONICITY: Monotonicity proof object.
          {e
          T1: (R t_1 s_1)
          ...
          Tn: (R t_n s_n)
          [monotonicity T1 ... Tn]: (R (f t_1 ... t_n) (f s_1 ... s_n))
          }
          Remark: if t_i == s_i, then the antecedent Ti is suppressed.
          That is, reflexivity proofs are supressed to save space.

   - OP_PR_QUANT_INTRO: Given a proof for (~ p q), produces a proof for (~ (forall (x) p) (forall (x) q)).

       T1: (~ p q)
       [quant-intro T1]: (~ (forall (x) p) (forall (x) q))
   
   - OP_PR_DISTRIBUTIVITY: Distributivity proof object. 
          Given that f (= or) distributes over g (= and), produces a proof for

          (= (f a (g c d))
             (g (f a c) (f a d)))

          If f and g are associative, this proof also justifies the following equality:

          (= (f (g a b) (g c d))
             (g (f a c) (f a d) (f b c) (f b d)))

          where each f and g can have arbitrary number of arguments.

          This proof object has no antecedents.
          Remark. This rule is used by the CNF conversion pass and 
          instantiated by f = or, and g = and.
    
   - OP_PR_AND_ELIM: Given a proof for (and l_1 ... l_n), produces a proof for l_i
        
       {e
       T1: (and l_1 ... l_n)
       [and-elim T1]: l_i
       }
   - OP_PR_NOT_OR_ELIM: Given a proof for (not (or l_1 ... l_n)), produces a proof for (not l_i).

       {e
       T1: (not (or l_1 ... l_n))
       [not-or-elim T1]: (not l_i)
       }

   - OP_PR_REWRITE: A proof for a local rewriting step (= t s).
          The head function symbol of t is interpreted.

          This proof object has no antecedents.
          The conclusion of a rewrite rule is either an equality (= t s), 
          an equivalence (iff t s), or equi-satisfiability (~ t s).
          Remark: if f is bool, then = is iff.
          

          Examples:
          {e
          (= (+ x 0) x)
          (= (+ x 1 2) (+ 3 x))
          (iff (or x false) x)
          }

   - OP_PR_REWRITE_STAR: A proof for rewriting an expression t into an expression s.
       This proof object is used if the parameter PROOF_MODE is 1.
       This proof object can have n antecedents.
       The antecedents are proofs for equalities used as substitution rules.
       The object is also used in a few cases if the parameter PROOF_MODE is 2.
       The cases are:
         - When applying contextual simplification (CONTEXT_SIMPLIFIER=true)
         - When converting bit-vectors to Booleans (BIT2BOOL=true)
         - When pulling ite expression up (PULL_CHEAP_ITE_TREES=true)

   - OP_PR_PULL_QUANT: A proof for (iff (f (forall (x) q(x)) r) (forall (x) (f (q x) r))). This proof object has no antecedents.

   - OP_PR_PULL_QUANT_STAR: A proof for (iff P Q) where Q is in prenex normal form.
       This proof object is only used if the parameter PROOF_MODE is 1.       
       This proof object has no antecedents.
  
   - OP_PR_PUSH_QUANT: A proof for:

       {e
          (iff (forall (x_1 ... x_m) (and p_1[x_1 ... x_m] ... p_n[x_1 ... x_m]))
               (and (forall (x_1 ... x_m) p_1[x_1 ... x_m])
                 ... 
               (forall (x_1 ... x_m) p_n[x_1 ... x_m])))
               }
         This proof object has no antecedents.

   - OP_PR_ELIM_UNUSED_VARS:  
          A proof for (iff (forall (x_1 ... x_n y_1 ... y_m) p[x_1 ... x_n])
                           (forall (x_1 ... x_n) p[x_1 ... x_n])) 

          It is used to justify the elimination of unused variables.
          This proof object has no antecedents.

   - OP_PR_DER: A proof for destructive equality resolution:
          (iff (forall (x) (or (not (= x t)) P[x])) P[t])
          if x does not occur in t.

          This proof object has no antecedents.
          
          Several variables can be eliminated simultaneously.

   - OP_PR_QUANT_INST: A proof of (or (not (forall (x) (P x))) (P a))

   - OP_PR_HYPOTHESIS: Mark a hypothesis in a natural deduction style proof.

   - OP_PR_LEMMA: 

       {e
          T1: false
          [lemma T1]: (or (not l_1) ... (not l_n))
          }
          This proof object has one antecedent: a hypothetical proof for false.
          It converts the proof in a proof for (or (not l_1) ... (not l_n)),
          when T1 contains the hypotheses: l_1, ..., l_n.

   - OP_PR_UNIT_RESOLUTION: 
       {e
          T1:      (or l_1 ... l_n l_1' ... l_m')
          T2:      (not l_1)
          ...
          T(n+1):  (not l_n)
          [unit-resolution T1 ... T(n+1)]: (or l_1' ... l_m')
          }

   - OP_PR_IFF_TRUE: 
      {e
       T1: p
       [iff-true T1]: (iff p true)
       }

   - OP_PR_IFF_FALSE:
      {e
       T1: (not p)
       [iff-false T1]: (iff p false)
       }

   - OP_PR_COMMUTATIVITY:

          [comm]: (= (f a b) (f b a))
          
          f is a commutative operator.

          This proof object has no antecedents.
          Remark: if f is bool, then = is iff.
   
   - OP_PR_DEF_AXIOM: Proof object used to justify Tseitin's like axioms:
       
          {e
          (or (not (and p q)) p)
          (or (not (and p q)) q)
          (or (not (and p q r)) p)
          (or (not (and p q r)) q)
          (or (not (and p q r)) r)
          ...
          (or (and p q) (not p) (not q))
          (or (not (or p q)) p q)
          (or (or p q) (not p))
          (or (or p q) (not q))
          (or (not (iff p q)) (not p) q)
          (or (not (iff p q)) p (not q))
          (or (iff p q) (not p) (not q))
          (or (iff p q) p q)
          (or (not (ite a b c)) (not a) b)
          (or (not (ite a b c)) a c)
          (or (ite a b c) (not a) (not b))
          (or (ite a b c) a (not c))
          (or (not (not a)) (not a))
          (or (not a) a)
          }
          This proof object has no antecedents.
          Note: all axioms are propositional tautologies.
          Note also that 'and' and 'or' can take multiple arguments.
          You can recover the propositional tautologies by
          unfolding the Boolean connectives in the axioms a small
          bounded number of steps (=3).
    
   - OP_PR_DEF_INTRO: Introduces a name for a formula/term.
       Suppose e is an expression with free variables x, and def-intro
       introduces the name n(x). The possible cases are:

       When e is of Boolean type:
       [def-intro]: (and (or n (not e)) (or (not n) e))

       or:
       [def-intro]: (or (not n) e)
       when e only occurs positively.

       When e is of the form (ite cond th el):
       [def-intro]: (and (or (not cond) (= n th)) (or cond (= n el)))

       Otherwise:
       [def-intro]: (= n e)       

   - OP_PR_APPLY_DEF: 
       [apply-def T1]: F ~ n
       F is 'equivalent' to n, given that T1 is a proof that
       n is a name for F.
   
   - OP_PR_IFF_OEQ:
       T1: (iff p q)
       [iff~ T1]: (~ p q)
 
   - OP_PR_NNF_POS: Proof for a (positive) NNF step. Example:
       {e
          T1: (not s_1) ~ r_1
          T2: (not s_2) ~ r_2
          T3: s_1 ~ r_1'
          T4: s_2 ~ r_2'
          [nnf-pos T1 T2 T3 T4]: (~ (iff s_1 s_2)
                                    (and (or r_1 r_2') (or r_1' r_2)))
          }
       The negation normal form steps NNF_POS and NNF_NEG are used in the following cases:
       (a) When creating the NNF of a positive force quantifier.
        The quantifier is retained (unless the bound variables are eliminated).
        Example
        {e
           T1: q ~ q_new 
           [nnf-pos T1]: (~ (forall (x T) q) (forall (x T) q_new))
        }
       (b) When recursively creating NNF over Boolean formulas, where the top-level
       connective is changed during NNF conversion. The relevant Boolean connectives
       for NNF_POS are 'implies', 'iff', 'xor', 'ite'.
       NNF_NEG furthermore handles the case where negation is pushed
       over Boolean connectives 'and' and 'or'.

    
   - OP_PR_NFF_NEG: Proof for a (negative) NNF step. Examples:
          {e
          T1: (not s_1) ~ r_1
          ...
          Tn: (not s_n) ~ r_n
         [nnf-neg T1 ... Tn]: (not (and s_1 ... s_n)) ~ (or r_1 ... r_n)
      and
          T1: (not s_1) ~ r_1
          ...
          Tn: (not s_n) ~ r_n
         [nnf-neg T1 ... Tn]: (not (or s_1 ... s_n)) ~ (and r_1 ... r_n)
      and
          T1: (not s_1) ~ r_1
          T2: (not s_2) ~ r_2
          T3: s_1 ~ r_1'
          T4: s_2 ~ r_2'
         [nnf-neg T1 T2 T3 T4]: (~ (not (iff s_1 s_2))
                                   (and (or r_1 r_2) (or r_1' r_2')))
       }
   - OP_PR_NNF_STAR: A proof for (~ P Q) where Q is in negation normal form.
       
       This proof object is only used if the parameter PROOF_MODE is 1.       
              
       This proof object may have n antecedents. Each antecedent is a PR_DEF_INTRO.

   - OP_PR_CNF_STAR: A proof for (~ P Q) where Q is in conjunctive normal form.
       This proof object is only used if the parameter PROOF_MODE is 1.       
       This proof object may have n antecedents. Each antecedent is a PR_DEF_INTRO.          

   - OP_PR_SKOLEMIZE: Proof for:  
       
          {e
          [sk]: (~ (not (forall x (p x y))) (not (p (sk y) y)))
          [sk]: (~ (exists x (p x y)) (p (sk y) y))
          }

          This proof object has no antecedents.
   
   - OP_PR_MODUS_PONENS_OEQ: Modus ponens style rule for equi-satisfiability.
       {e
          T1: p
          T2: (~ p q)
          [mp~ T1 T2]: q
          }

    - OP_PR_TH_LEMMA: Generic proof for theory lemmas.

         The theory lemma function comes with one or more parameters.
         The first parameter indicates the name of the theory.
         For the theory of arithmetic, additional parameters provide hints for
         checking the theory lemma. 
         The hints for arithmetic are:
         
         - farkas - followed by rational coefficients. Multiply the coefficients to the
           inequalities in the lemma, add the (negated) inequalities and obtain a contradiction.

         - triangle-eq - Indicates a lemma related to the equivalence:
         {e
            (iff (= t1 t2) (and (<= t1 t2) (<= t2 t1)))
         }

         - gcd-test - Indicates an integer linear arithmetic lemma that uses a gcd test.


      - OP_RA_STORE: Insert a record into a relation.
        The function takes [n+1] arguments, where the first argument is the relation and the remaining [n] elements 
        correspond to the [n] columns of the relation.

      - OP_RA_EMPTY: Creates the empty relation. 
        
      - OP_RA_IS_EMPTY: Tests if the relation is empty.

      - OP_RA_JOIN: Create the relational join.

      - OP_RA_UNION: Create the union or convex hull of two relations. 
        The function takes two arguments.

      - OP_RA_WIDEN: Widen two relations.
        The function takes two arguments.

      - OP_RA_PROJECT: Project the columns (provided as numbers in the parameters).
        The function takes one argument.

      - OP_RA_FILTER: Filter (restrict) a relation with respect to a predicate.
        The first argument is a relation. 
        The second argument is a predicate with free de-Brujin indices
        corresponding to the columns of the relation.
        So the first column in the relation has index 0.

      - OP_RA_NEGATION_FILTER: Intersect the first relation with respect to negation
        of the second relation (the function takes two arguments).
        Logically, the specification can be described by a function

           target = filter_by_negation(pos, neg, columns)

        where columns are pairs c1, d1, .., cN, dN of columns from pos and neg, such that
        target are elements in x in pos, such that there is no y in neg that agrees with
        x on the columns c1, d1, .., cN, dN.

    
      - OP_RA_RENAME: rename columns in the relation. 
        The function takes one argument.
        The parameters contain the renaming as a cycle.
         
      - OP_RA_COMPLEMENT: Complement the relation.

      - OP_RA_SELECT: Check if a record is an element of the relation.
        The function takes [n+1] arguments, where the first argument is a relation,
        and the remaining [n] arguments correspond to a record.

      - OP_RA_CLONE: Create a fresh copy (clone) of a relation. 
        The function is logically the identity, but
        in the context of a register machine allows
        for  [OP_RA_UNION]  
        to perform destructive updates to the first argument.
        

      - OP_FD_LT: A less than predicate over the finite domain FINITE_DOMAIN_SORT.

      - OP_LABEL: A label (used by the Boogie Verification condition generator).
                     The label has two parameters, a string and a Boolean polarity.
                     It takes one argument, a formula.

      - OP_LABEL_LIT: A label literal (used by the Boogie Verification condition generator).
                     A label literal has a set of string parameters. It takes no arguments.

      - OP_UNINTERPRETED: kind used for uninterpreted symbols.

*)
(**
    {!search_failure}  
   The different kinds of search failure types.

   - NO_FAILURE:         The last search was successful
   - UNKNOWN:            Undocumented failure reason
   - TIMEOUT:            Timeout
   - MEMOUT_WATERMAK:    Search hit a memory high-watermak limit
   - CANCELED:           val cancel flag was set
   - NUM_CONFLICTS:      Maximum number of conflicts was reached
   - THEORY:             Theory is incomplete
   - QUANTIFIERS:        Logical context contains universal quantifiers
*)
(**
    {!ast_print_mode}  
   Z3 pretty printing modes (See {!set_ast_print_mode}).

   - PRINT_SMTLIB_FULL:   Print AST nodes in SMTLIB verbose format.
   - PRINT_LOW_LEVEL:     Print AST nodes using a low-level format.
   - PRINT_SMTLIB_COMPLIANT: Print AST nodes in SMTLIB 1.x compliant format.
   - PRINT_SMTLIB2_COMPLIANT: Print AST nodes in SMTLIB 2.x compliant format.
*)
(**
        {2 {L Create configuration}}
*)
(**
       Summary: Create a configuration.

       Configurations are created in order to assign parameters prior to creating 
       contexts for Z3 interaction. For example, if the users wishes to use model
       generation, then call:

       [set_param_value cfg "MODEL" "true"]

        - {b Remarks}: Consider using {!mk_context_x} instead of using
       explicit configuration objects. The function {!mk_context_x}
       receives an array of string pairs. This array represents the
       configuration options. 

       - {b See also}: {!set_param_value}
       - {b See also}: {!del_config}
*)
val mk_config : unit -> config
	

(**
       Summary: Delete the given configuration object.

       - {b See also}: {!mk_config}
*)
val del_config : config -> unit
	

(**
       Summary: Set a configuration parameter.

       The list of all configuration parameters can be obtained using the Z3 executable:

       {v 
       z3.exe -ini?
        v}

       - {b See also}: {!mk_config}
*)
val set_param_value : config -> string -> string -> unit
	

(**
       {2 {L Create context}}
*)
(**
       Summary: Create a context using the given configuration. 
    
       After a context is created, the configuration cannot be changed,
       although some parameters can be changed using {!update_param_value}.
       All main interaction with Z3 happens in the context of a [context].

       

       
*)
val mk_context : config -> context
	

(**
       Summary: Delete the given logical context.

       - {b See also}: {!mk_context}
*)
val del_context : context -> unit
	

(**
       Summary: Update a mutable configuration parameter.

       The list of all configuration parameters can be obtained using the Z3 executable:

       {v 
       z3.exe -ini?
        v}

       Only a few configuration parameters are mutable once the context is created.
       The error handler is invoked when trying to modify an immutable parameter.

       
        - {b See also}: {!mk_context }
*)
val update_param_value : context -> string -> string -> unit
	

(**
       Summary: Get a configuration parameter.
      
       Returns  [None]  
       if the parameter value does not exist.

       
       
        - {b See also}: {!mk_context }
*)
val get_param_value : context -> string -> string option
	

(**
       {2 {L Symbols}}
*)
(**
        {4 {L Redundant low-level API}} 
*)
(**
       Summary: Create a Z3 symbol using an integer.

       Symbols are used to name several term and type constructors.

       NB. Not all integers can be passed to this function.
       The legal range of unsigned int integers is 0 to 2^30-1.

       - {b See also}: {!mk_string_symbol}
*)
val mk_int_symbol : context -> int -> symbol
	

(**
       Summary: Create a Z3 symbol using a C string.

       Symbols are used to name several term and type constructors.

       - {b See also}: {!mk_int_symbol}
*)
val mk_string_symbol : context -> string -> symbol
	

(**
       {2 {L Sorts}}
*)
(**
        {4 {L Redundant low-level API}} 
*)
(**
       Summary: Create a free (uninterpreted) type using the given name (symbol).
       
       Two free types are considered the same iff the have the same name.
*)
val mk_uninterpreted_sort : context -> symbol -> sort
	

(**
       Summary: Create the Boolean type. 

       This type is used to create propositional variables and predicates.
*)
val mk_bool_sort : context -> sort
	

(**
       Summary: Create the integer type.

       This type is not the int type found in programming languages.
       A machine integer can be represented using bit-vectors. The function
       {!mk_bv_sort} creates a bit-vector type.

       - {b See also}: {!mk_bv_sort}
*)
val mk_int_sort : context -> sort
	

(**
       Summary: Create the real type. 

       This type is not a floating point number.
       Z3 does not have support for floating point numbers yet.
*)
val mk_real_sort : context -> sort
	

(**
       Summary: Create a bit-vector type of the given size.
    
       This type can also be seen as a machine integer.

       - {b Remarks}: The size of the bitvector type must be greater than zero.
*)
val mk_bv_sort : context -> int -> sort
	

(**
       Summary: Create a named finite domain sort.

       To create constants that belong to the finite domain, 
       use the APIs for creating numerals and pass a numeric
       constant together with the sort returned by this call.

       - {b See also}: {!get_finite_domain_sort_size.}
*)
val mk_finite_domain_sort : context -> symbol -> int64 -> sort
	

(**
       Summary: Create an array type. 
       
       We usually represent the array type as: {e [domain -> range] }.
       Arrays are usually used to model the heap/memory in software verification.

       - {b See also}: {!mk_select}
       - {b See also}: {!mk_store}
*)
val mk_array_sort : context -> sort -> sort -> sort
	

(**
       Summary: Create a tuple type.
       
        [mk_tuple_sort c name field_names field_sorts] creates a tuple with a constructor named [name],
       a [n] fields, where [n] is the size of the arrays [field_names] and [field_sorts].
       

       
       

       @param c logical context
       @param mk_tuple_name name of the constructor function associated with the tuple type.
       @param num_fields number of fields in the tuple type.
       @param field_names name of the projection functions.
       @param field_sorts type of the tuple fields.
       @param mk_tuple_decl output parameter that will contain the constructor declaration.
       @param proj_decl output parameter that will contain the projection function declarations. This field must be a buffer of size [num_fields] allocated by the user.
*)
val mk_tuple_sort : context -> symbol -> symbol array -> sort array -> sort * func_decl * func_decl array
	

(**
       Summary: Create a enumeration sort.
       
        [mk_enumeration_sort c enums] creates an enumeration sort with enumeration names [enums], 
               it also returns [n] predicates, where [n] is the number of [enums] corresponding
               to testing whether an element is one of the enumerants.
       

       
       
       
       @param c logical context
       @param name name of the enumeration sort.
       @param n number of elemenets in enumeration sort.
       @param enum_names names of the enumerated elements.
       @param enum_consts constants corresponding to the enumerated elements.
       @param enum_testers predicates testing if terms of the enumeration sort correspond to an enumeration.

       For example, if this function is called with three symbols A, B, C and the name S, then 
       [s] is a sort whose name is S, and the function returns three terms corresponding to A, B, C in 
       [enum_consts]. The array [enum_testers] has three predicates of type {e (s -> Bool) }.
       The first predicate (corresponding to A) is true when applied to A, and false otherwise.
       Similarly for the other predicates.
*)
val mk_enumeration_sort : context -> symbol -> symbol array -> sort * func_decl array * func_decl array
	

(**
       Summary: Create a list sort
       
        [mk_list_sort c name elem_sort] creates a list sort of [name], over elements of sort [elem_sort].
       

       
       

       @param c logical context
       @param name name of the list sort.
       @param elem_sort sort of list elements.
       @param nil_decl declaration for the empty list.
       @param is_nil_decl test for the empty list.
       @param cons_decl declaration for a cons cell.
       @param is_cons_decl cons cell test.
       @param head_decl list head.
       @param tail_decl list tail.
*)
val mk_list_sort : context -> symbol -> sort -> sort * func_decl * func_decl * func_decl * func_decl * func_decl * func_decl
	

(**
       Summary: Create a constructor.
       
       @param c logical context.
       @param name constructor name.
       @param recognizer name of recognizer function.
       @param num_fields number of fields in constructor.
       @param field_names names of the constructor fields.
       @param sorts field sorts,  [None]  
                    if the field sort refers to a recursive sort.
       @param sort_refs reference to datatype sort that is an argument to the constructor; if the corresponding
                        sort reference is  [None],  
                        then the value in sort_refs should be an index referring to 
                        one of the recursive datatypes that is declared.                        
*)
val mk_constructor : context -> symbol -> symbol -> symbol array -> sort array -> int array -> constructor
	

(**
       Summary: Reclaim memory allocated to constructor.

       @param c logical context.
       @param constr constructor.
*)
val del_constructor : context -> constructor -> unit
	

(**
       Summary: Create datatype, such as lists, trees, records, enumerations or unions of records. 
       The datatype may be recursive. Return the datatype sort.

       @param c logical context.
	   @param name name of datatype.
       @param num_constructors number of constructors passed in.
       @param constructors array of constructor containers.
*)
val mk_datatype : context -> symbol -> constructor array -> sort * constructor array
	

(**
       Summary: Create list of constructors.

       @param c logical context.
       @param num_constructors number of constructors in list.
       @param constructors list of constructors.
*)
val mk_constructor_list : context -> constructor array -> constructor_list
	

(**
       Summary: Reclaim memory allocated for constructor list.

       Each constructor inside the constructor list must be independently reclaimed using {!del_constructor}.

       @param c logical context.
       @param clist constructor list container.

*)
val del_constructor_list : context -> constructor_list -> unit
	

(**
       Summary: Create mutually recursive datatypes.

       @param c logical context.
       @param num_sorts number of datatype sorts.
       @param sort_names names of datatype sorts.
       @param sorts array of datattype sorts.
       @param constructor_lists list of constructors, one list per sort.
*)
val mk_datatypes : context -> symbol array -> constructor_list array -> sort array * constructor_list array
	

(**
       Summary: Query constructor for declared functions. 
      
       @param c logical context.
       @param constr constructor container. The container must have been passed in to a {!mk_datatype} call.
       @param num_fields number of accessor fields in the constructor.
       @param constructor constructor function declaration.
       @param tester constructor test function declaration.
       @param accessors array of accessor function declarations.
*)
val query_constructor : context -> constructor -> int -> func_decl * func_decl * func_decl array
	

(**
       {2 {L Constants and Applications}}
*)
(**
       Summary: Declare a constant or function.

        [mk_func_decl c n d r] creates a function with name [n], domain [d], and range [r].
       The arity of the function is the size of the array [d]. 

       @param c logical context.
       @param s name of the constant or function.
       @param domain_size number of arguments. It is 0 when declaring a constant.
       @param domain array containing the sort of each argument. The array must contain domain_size elements. It is 0 when declaring a constant.
       @param range sort of the constant or the return sort of the function.

       After declaring a constant or function, the function
       {!mk_app} can be used to create a constant or function
       application.

       - {b See also}: {!mk_app}
*)
val mk_func_decl : context -> symbol -> sort array -> sort -> func_decl
	

(**
       Summary: Create a constant or function application.

       - {b See also}: {!mk_func_decl}
*)
val mk_app : context -> func_decl -> ast array -> ast
	

(**
       Summary: Declare and create a constant.
       
       
       
       
       
       
       
        [mk_const c s t] is a shorthand for [mk_app c (mk_func_decl c s [||] t) [||]] 

       - {b See also}: {!mk_func_decl}
       - {b See also}: {!mk_app}
*)
val mk_const : context -> symbol -> sort -> ast
	

(**
       Summary: Declare a fresh constant or function.

       Z3 will generate an unique name for this function declaration.
       
       
       

       - {b See also}: {!mk_func_decl}
*)
val mk_fresh_func_decl : context -> string -> sort array -> sort -> func_decl
	

(**
       Summary: Declare and create a fresh constant.
       
       
       

        [mk_fresh_const c p t] is a shorthand for [mk_app c (mk_fresh_func_decl c p [||] t) [||]]. 

       
       
       - {b See also}: {!mk_func_decl}
       - {b See also}: {!mk_app}
*)
val mk_fresh_const : context -> string -> sort -> ast
	

(**
       {2 {L Propositional Logic and Equality}}
*)
(**
        Summary: Create an AST node representing [true].
*)
val mk_true : context -> ast
	

(**
        Summary: Create an AST node representing [false].
*)
val mk_false : context -> ast
	

(**
        Summary: \[ [ mk_eq c l r ] \]
        Create an AST node representing {e l = r }.
        
        The nodes [l] and [r] must have the same type. 
*)
val mk_eq : context -> ast -> ast -> ast
	

(**
       
        Summary: \[ [mk_distinct c [| t_1; ...; t_n |]] \] Create an AST
       node represeting a distinct construct. It is used for declaring
       the arguments t_i pairwise distinct. 

       The [distinct] construct is used for declaring the arguments pairwise distinct.
       That is, {e Forall 0 <= i < j < num_args. not args[i] = args[j] }.
       
       All arguments must have the same sort.

       - {b Remarks}: The number of arguments of a distinct construct must be greater than one.
*)
val mk_distinct : context -> ast array -> ast
	

(**
        Summary: \[ [ mk_not c a ] \] 
        Create an AST node representing {e not(a) }.
        
        The node [a] must have Boolean sort.
*)
val mk_not : context -> ast -> ast
	

(**
       Summary: \[ [ mk_ite c t1 t2 t2 ] \] 
       Create an AST node representing an if-then-else: {e ite(t1, t2,
       t3) }.

       The node [t1] must have Boolean sort, [t2] and [t3] must have the same sort.
       The sort of the new node is equal to the sort of [t2] and [t3].
*)
val mk_ite : context -> ast -> ast -> ast -> ast
	

(**
       Summary: \[ [ mk_iff c t1 t2 ] \]
       Create an AST node representing {e t1 iff t2 }.

       The nodes [t1] and [t2] must have Boolean sort.
*)
val mk_iff : context -> ast -> ast -> ast
	

(**
       Summary: \[ [ mk_implies c t1 t2 ] \]
       Create an AST node representing {e t1 implies t2 }.

       The nodes [t1] and [t2] must have Boolean sort.
*)
val mk_implies : context -> ast -> ast -> ast
	

(**
       Summary: \[ [ mk_xor c t1 t2 ] \]
       Create an AST node representing {e t1 xor t2 }.

       The nodes [t1] and [t2] must have Boolean sort.
*)
val mk_xor : context -> ast -> ast -> ast
	

(**
       
        Summary: \[ [mk_and c [| t_1; ...; t_n |]] \] Create the conjunction: {e t_1 and ... and t_n}. 

       
       All arguments must have Boolean sort.
       
       - {b Remarks}: The number of arguments must be greater than zero.
*)
val mk_and : context -> ast array -> ast
	

(**
       
        Summary: \[ [mk_or c [| t_1; ...; t_n |]] \] Create the disjunction: {e t_1 or ... or t_n}. 

       
       All arguments must have Boolean sort.

       - {b Remarks}: The number of arguments must be greater than zero.
*)
val mk_or : context -> ast array -> ast
	

(**
       {2 {L Arithmetic: Integers and Reals}}
*)
(**
       
        Summary: \[ [mk_add c [| t_1; ...; t_n |]] \] Create the term: {e t_1 + ... + t_n}. 

       
       All arguments must have int or real sort.

       - {b Remarks}: The number of arguments must be greater than zero.
*)
val mk_add : context -> ast array -> ast
	

(**
       
        Summary: \[ [mk_mul c [| t_1; ...; t_n |]] \] Create the term: {e t_1 * ... * t_n}. 

       
       All arguments must have int or real sort.
       
       - {b Remarks}: Z3 has limited support for non-linear arithmetic.
       - {b Remarks}: The number of arguments must be greater than zero.
*)
val mk_mul : context -> ast array -> ast
	

(**
       
        Summary: \[ [mk_sub c [| t_1; ...; t_n |]] \] Create the term: {e t_1 - ... - t_n}. 

       
       All arguments must have int or real sort.

       - {b Remarks}: The number of arguments must be greater than zero.
*)
val mk_sub : context -> ast array -> ast
	

(**
       
        Summary: \[ [mk_unary_minus c arg] \] Create the term: {e - arg}. 

       The arguments must have int or real type.
*)
val mk_unary_minus : context -> ast -> ast
	

(**
       
        Summary: \[ [mk_div c t_1 t_2] \] Create the term: {e t_1 div t_2}. 

       The arguments must either both have int type or both have real type.
       If the arguments have int type, then the result type is an int type, otherwise the
       the result type is real.

*)
val mk_div : context -> ast -> ast -> ast
	

(**
       
        Summary: \[ [mk_mod c t_1 t_2] \] Create the term: {e t_1 mod t_2}. 

       The arguments must have int type.

*)
val mk_mod : context -> ast -> ast -> ast
	

(**
       
        Summary: \[ [mk_rem c t_1 t_2] \] Create the term: {e t_1 rem t_2}. 

       The arguments must have int type.

*)
val mk_rem : context -> ast -> ast -> ast
	

(**
       

       The arguments must have int or real type.
*)
val mk_power : context -> ast -> ast -> ast
	

(**
        Summary: \[ [ mk_lt c t1 t2 ] \] 
        Create less than.

        The nodes [t1] and [t2] must have the same sort, and must be int or real.
*)
val mk_lt : context -> ast -> ast -> ast
	

(**
        Summary: \[ [ mk_le c t1 t2 ] \]
        Create less than or equal to.
        
        The nodes [t1] and [t2] must have the same sort, and must be int or real.
*)
val mk_le : context -> ast -> ast -> ast
	

(**
        Summary: \[ [ mk_gt c t1 t2 ] \]
        Create greater than.
        
        The nodes [t1] and [t2] must have the same sort, and must be int or real.
*)
val mk_gt : context -> ast -> ast -> ast
	

(**
        Summary: \[ [ mk_ge c t1 t2 ] \]
        Create greater than or equal to.
        
        The nodes [t1] and [t2] must have the same sort, and must be int or real.
*)
val mk_ge : context -> ast -> ast -> ast
	

(**
        Summary: \[ [ mk_int2real c t1 ] \]
        Coerce an integer to a real.

        There is also a converse operation exposed.
        It follows the semantics prescribed by the SMT-LIB standard.

        You can take the floor of a real by 
        creating an auxiliary integer constant [k] and
        and asserting {e  mk_int2real(k) <= t1 < mk_int2real(k)+1 }.
        
        The node [t1] must have sort integer.

        - {b See also}: {!mk_real2int}
        - {b See also}: {!mk_is_int}
*)
val mk_int2real : context -> ast -> ast
	

(**
        Summary: \[ [ mk_real2int c t1 ] \]
        Coerce a real to an integer.

        The semantics of this function follows the SMT-LIB standard
        for the function to_int

        - {b See also}: {!mk_int2real}
        - {b See also}: {!mk_is_int}
*)
val mk_real2int : context -> ast -> ast
	

(**
        Summary: \[ [ mk_is_int c t1 ] \]
        Check if a real number is an integer.

        - {b See also}: {!mk_int2real}
        - {b See also}: {!mk_real2int}
*)
val mk_is_int : context -> ast -> ast
	

(**
       {2 {L Bit-vectors}}
*)
(**
       Summary: \[ [ mk_bvnot c t1 ] \]
       Bitwise negation.

       The node [t1] must have a bit-vector sort.
*)
val mk_bvnot : context -> ast -> ast
	

(**
       Summary: \[ [ mk_bvredand c t1 ] \]
       Take conjunction of bits in vector, return vector of length 1.

       The node [t1] must have a bit-vector sort.
*)
val mk_bvredand : context -> ast -> ast
	

(**
       Summary: \[ [ mk_bvredor c t1 ] \]
       Take disjunction of bits in vector, return vector of length 1.

       The node [t1] must have a bit-vector sort.
*)
val mk_bvredor : context -> ast -> ast
	

(**
       Summary: \[ [ mk_bvand c t1 t2 ] \]
       Bitwise and.

       The nodes [t1] and [t2] must have the same bit-vector sort.
*)
val mk_bvand : context -> ast -> ast -> ast
	

(**
       Summary: \[ [ mk_bvor c t1 t2 ] \]
       Bitwise or.

       The nodes [t1] and [t2] must have the same bit-vector sort.
*)
val mk_bvor : context -> ast -> ast -> ast
	

(**
       Summary: \[ [ mk_bvxor c t1 t2 ] \]
       Bitwise exclusive-or.

       The nodes [t1] and [t2] must have the same bit-vector sort.
*)
val mk_bvxor : context -> ast -> ast -> ast
	

(**
       Summary: \[ [ mk_bvnand c t1 t2 ] \]
       Bitwise nand. 

       The nodes [t1] and [t2] must have the same bit-vector sort.
*)
val mk_bvnand : context -> ast -> ast -> ast
	

(**
       Summary: \[ [ mk_bvnor c t1 t2 ] \]
       Bitwise nor. 

       The nodes [t1] and [t2] must have the same bit-vector sort.
*)
val mk_bvnor : context -> ast -> ast -> ast
	

(**
       Summary: \[ [ mk_bvxnor c t1 t2 ] \]
       Bitwise xnor. 
       
       The nodes [t1] and [t2] must have the same bit-vector sort.
*)
val mk_bvxnor : context -> ast -> ast -> ast
	

(**
       Summary: \[ [ mk_bvneg c t1 ] \]
       Standard two's complement unary minus. 

       The node [t1] must have bit-vector sort.
*)
val mk_bvneg : context -> ast -> ast
	

(**
        Summary: \[ [ mk_bvadd c t1 t2 ] \]
        Standard two's complement addition.
        
        The nodes [t1] and [t2] must have the same bit-vector sort.
*)
val mk_bvadd : context -> ast -> ast -> ast
	

(**
        Summary: \[ [ mk_bvsub c t1 t2 ] \]
        Standard two's complement subtraction.
        
        The nodes [t1] and [t2] must have the same bit-vector sort.
*)
val mk_bvsub : context -> ast -> ast -> ast
	

(**
        Summary: \[ [ mk_bvmul c t1 t2 ] \]
        Standard two's complement multiplication.
        
        The nodes [t1] and [t2] must have the same bit-vector sort.
*)
val mk_bvmul : context -> ast -> ast -> ast
	

(**
        Summary: \[ [ mk_bvudiv c t1 t2 ] \]
        Unsigned division. 

        It is defined as the [floor] of {e t1/t2 } if [t2] is
        different from zero. If {e t2 } is zero, then the result
        is undefined.
        
        The nodes [t1] and [t2] must have the same bit-vector sort.
*)
val mk_bvudiv : context -> ast -> ast -> ast
	

(**
        Summary: \[ [ mk_bvsdiv c t1 t2 ] \]
        Two's complement signed division. 

        It is defined in the following way:

        - The [floor] of {e t1/t2 } if [t2] is different from zero, and {e t1*t2 >= 0 }.

        - The [ceiling] of {e t1/t2 } if [t2] is different from zero, and {e t1*t2 < 0 }.
        
        If {e t2 } is zero, then the result is undefined.
        
        The nodes [t1] and [t2] must have the same bit-vector sort.
*)
val mk_bvsdiv : context -> ast -> ast -> ast
	

(**
       Summary: \[ [ mk_bvurem c t1 t2 ] \]
       Unsigned remainder.

       It is defined as {e t1 - (t1 /u t2) * t2 }, where {e /u } represents unsigned int division.
       
       If {e t2 } is zero, then the result is undefined.
       
       The nodes [t1] and [t2] must have the same bit-vector sort.
*)
val mk_bvurem : context -> ast -> ast -> ast
	

(**
       Summary: \[ [ mk_bvsrem c t1 t2 ] \]
       Two's complement signed remainder (sign follows dividend).

       It is defined as {e t1 - (t1 /s t2) * t2 }, where {e /s } represents signed division.
       The most significant bit (sign) of the result is equal to the most significant bit of [t1].

       If {e t2 } is zero, then the result is undefined.
       
       The nodes [t1] and [t2] must have the same bit-vector sort.

       - {b See also}: {!mk_bvsmod}
*)
val mk_bvsrem : context -> ast -> ast -> ast
	

(**
       Summary: \[ [ mk_bvsmod c t1 t2 ] \]
       Two's complement signed remainder (sign follows divisor).
       
       If {e t2 } is zero, then the result is undefined.
       
       The nodes [t1] and [t2] must have the same bit-vector sort.

       - {b See also}: {!mk_bvsrem}
*)
val mk_bvsmod : context -> ast -> ast -> ast
	

(**
       Summary: \[ [ mk_bvult c t1 t2 ] \]
       Unsigned less than.

       The nodes [t1] and [t2] must have the same bit-vector sort.
*)
val mk_bvult : context -> ast -> ast -> ast
	

(**
       Summary: \[ [ mk_bvslt c t1 t2 ] \]
       Two's complement signed less than.
       
       It abbreviates:
       {v 
      (or (and (= (extract[|m-1|:|m-1|] t1) bit1)
               (= (extract[|m-1|:|m-1|] t2) bit0))
          (and (= (extract[|m-1|:|m-1|] t1) (extract[|m-1|:|m-1|] t2))
               (bvult t1 t2)))
        v}

       The nodes [t1] and [t2] must have the same bit-vector sort.
*)
val mk_bvslt : context -> ast -> ast -> ast
	

(**
       Summary: \[ [ mk_bvule c t1 t2 ] \]
       Unsigned less than or equal to.

       The nodes [t1] and [t2] must have the same bit-vector sort.
*)
val mk_bvule : context -> ast -> ast -> ast
	

(**
       Summary: \[ [ mk_bvsle c t1 t2 ] \]
       Two's complement signed less than or equal to.

       The nodes [t1] and [t2] must have the same bit-vector sort.
*)
val mk_bvsle : context -> ast -> ast -> ast
	

(**
       Summary: \[ [ mk_bvuge c t1 t2 ] \]
       Unsigned greater than or equal to.

       The nodes [t1] and [t2] must have the same bit-vector sort.
*)
val mk_bvuge : context -> ast -> ast -> ast
	

(**
       Summary: \[ [ mk_bvsge c t1 t2 ] \]
       Two's complement signed greater than or equal to.

       The nodes [t1] and [t2] must have the same bit-vector sort.
*)
val mk_bvsge : context -> ast -> ast -> ast
	

(**
       Summary: \[ [ mk_bvugt c t1 t2 ] \]
       Unsigned greater than.

       The nodes [t1] and [t2] must have the same bit-vector sort.
*)
val mk_bvugt : context -> ast -> ast -> ast
	

(**
       Summary: \[ [ mk_bvsgt c t1 t2 ] \]
       Two's complement signed greater than.

       The nodes [t1] and [t2] must have the same bit-vector sort.
*)
val mk_bvsgt : context -> ast -> ast -> ast
	

(**
       Summary: \[ [ mk_concat c t1 t2 ] \]
       Concatenate the given bit-vectors.
       
       The nodes [t1] and [t2] must have (possibly different) bit-vector sorts

       The result is a bit-vector of size {e n1+n2 }, where [n1] ([n2)] is the size
       of [t1] ([t2)].
*)
val mk_concat : context -> ast -> ast -> ast
	

(**
       Summary: \[ [ mk_extract c high low t1 ] \]
       Extract the bits [high] down to [low] from a bitvector of
       size [m] to yield a new bitvector of size [n], where {e n =
       high - low + 1 }.

       The node [t1] must have a bit-vector sort.
*)
val mk_extract : context -> int -> int -> ast -> ast
	

(**
       Summary: \[ [ mk_sign_ext c i t1 ] \]
       Sign-extend of the given bit-vector to the (signed) equivalent bitvector of
       size {e m+i }, where [m] is the size of the given
       bit-vector.

       The node [t1] must have a bit-vector sort.
*)
val mk_sign_ext : context -> int -> ast -> ast
	

(**
       Summary: \[ [ mk_zero_ext c i t1 ] \]
       Extend the given bit-vector with zeros to the (unsigned int) equivalent
       bitvector of size {e m+i }, where [m] is the size of the
       given bit-vector.
       
       The node [t1] must have a bit-vector sort. 
*)
val mk_zero_ext : context -> int -> ast -> ast
	

(**
       Summary: \[ [ mk_repeat c i t1 ] \]
       Repeat the given bit-vector up length {e i }.
       
       The node [t1] must have a bit-vector sort. 
*)
val mk_repeat : context -> int -> ast -> ast
	

(**
       Summary: \[ [ mk_bvshl c t1 t2 ] \]
       Shift left.

       It is equivalent to multiplication by {e 2^x } where [x] is the value of the
       third argument.

       NB. The semantics of shift operations varies between environments. This 
       definition does not necessarily capture directly the semantics of the 
       programming language or assembly architecture you are modeling.

       The nodes [t1] and [t2] must have the same bit-vector sort.
*)
val mk_bvshl : context -> ast -> ast -> ast
	

(**
       Summary: \[ [ mk_bvlshr c t1 t2 ] \]
       Logical shift right.

       It is equivalent to unsigned int division by {e 2^x } where [x] is the
       value of the third argument.

       NB. The semantics of shift operations varies between environments. This 
       definition does not necessarily capture directly the semantics of the 
       programming language or assembly architecture you are modeling.

       The nodes [t1] and [t2] must have the same bit-vector sort.
*)
val mk_bvlshr : context -> ast -> ast -> ast
	

(**
       Summary: \[ [ mk_bvashr c t1 t2 ] \]
       Arithmetic shift right.
       
       It is like logical shift right except that the most significant
       bits of the result always copy the most significant bit of the
       second argument.

       NB. The semantics of shift operations varies between environments. This 
       definition does not necessarily capture directly the semantics of the 
       programming language or assembly architecture you are modeling.
       
       The nodes [t1] and [t2] must have the same bit-vector sort.
*)
val mk_bvashr : context -> ast -> ast -> ast
	

(**
       Summary: \[ [ mk_rotate_left c i t1 ] \]
       Rotate bits of [t1] to the left [i] times.
       
       The node [t1] must have a bit-vector sort. 
*)
val mk_rotate_left : context -> int -> ast -> ast
	

(**
       Summary: \[ [ mk_rotate_right c i t1 ] \]
       Rotate bits of [t1] to the right [i] times.
       
       The node [t1] must have a bit-vector sort. 
*)
val mk_rotate_right : context -> int -> ast -> ast
	

(**
       Summary: \[ [ mk_ext_rotate_left c t1 t2 ] \]
       Rotate bits of [t1] to the left [t2] times.
       
       The nodes [t1] and [t2] must have the same bit-vector sort.
*)
val mk_ext_rotate_left : context -> ast -> ast -> ast
	

(**
       Summary: \[ [ mk_ext_rotate_right c t1 t2 ] \]
       Rotate bits of [t1] to the right [t2] times.
       
       The nodes [t1] and [t2] must have the same bit-vector sort.
*)
val mk_ext_rotate_right : context -> ast -> ast -> ast
	

(**
       Summary: \[ [ mk_int2bv c n t1 ] \]
       Create an [n] bit bit-vector from the integer argument [t1].

       NB. This function is essentially treated as uninterpreted. 
       So you cannot expect Z3 to precisely reflect the semantics of this function
       when solving constraints with this function.
       
       The node [t1] must have integer sort. 
*)
val mk_int2bv : context -> int -> ast -> ast
	

(**
       Summary: \[ [ mk_bv2int c t1 is_signed ] \]
       Create an integer from the bit-vector argument [t1].
       If [is_signed] is false, then the bit-vector [t1] is treated as unsigned int. 
       So the result is non-negative
       and in the range {e [0..2^N-1] }, where N are the number of bits in [t1].
       If [is_signed] is true, [t1] is treated as a signed bit-vector.

       NB. This function is essentially treated as uninterpreted. 
       So you cannot expect Z3 to precisely reflect the semantics of this function
       when solving constraints with this function.

       The node [t1] must have a bit-vector sort. 
*)
val mk_bv2int : context -> ast -> bool -> ast
	

(**
       Summary: \[ [ mk_bvadd_no_overflow c t1 t2 is_signed ] \]
       Create a predicate that checks that the bit-wise addition
       of [t1] and [t2] does not overflow.
       
       The nodes [t1] and [t2] must have the same bit-vector sort.
*)
val mk_bvadd_no_overflow : context -> ast -> ast -> bool -> ast
	

(**
       Summary: \[ [ mk_bvadd_no_underflow c t1 t2 ] \]
       Create a predicate that checks that the bit-wise signed addition
       of [t1] and [t2] does not underflow.
       
       The nodes [t1] and [t2] must have the same bit-vector sort.
*)
val mk_bvadd_no_underflow : context -> ast -> ast -> ast
	

(**
       Summary: \[ [ mk_bvsub_no_overflow c t1 t2 ] \]
       Create a predicate that checks that the bit-wise signed subtraction
       of [t1] and [t2] does not overflow.
       
       The nodes [t1] and [t2] must have the same bit-vector sort.
*)
val mk_bvsub_no_overflow : context -> ast -> ast -> ast
	

(**
       Summary: \[ [ mk_bvsub_no_underflow c t1 t2 is_signed ] \]
       Create a predicate that checks that the bit-wise subtraction
       of [t1] and [t2] does not underflow.
       
       The nodes [t1] and [t2] must have the same bit-vector sort.
*)
val mk_bvsub_no_underflow : context -> ast -> ast -> bool -> ast
	

(**
       Summary: \[ [ mk_bvsdiv_no_overflow c t1 t2 ] \]
       Create a predicate that checks that the bit-wise signed division 
       of [t1] and [t2] does not overflow.
       
       The nodes [t1] and [t2] must have the same bit-vector sort.
*)
val mk_bvsdiv_no_overflow : context -> ast -> ast -> ast
	

(**
       Summary: \[ [ mk_bvneg_no_overflow c t1 ] \]
       Check that bit-wise negation does not overflow when 
       [t1] is interpreted as a signed bit-vector.
       
       The node [t1] must have bit-vector sort.
*)
val mk_bvneg_no_overflow : context -> ast -> ast
	

(**
       Summary: \[ [ mk_bvmul_no_overflow c t1 t2 is_signed ] \]
       Create a predicate that checks that the bit-wise multiplication
       of [t1] and [t2] does not overflow.
       
       The nodes [t1] and [t2] must have the same bit-vector sort.
*)
val mk_bvmul_no_overflow : context -> ast -> ast -> bool -> ast
	

(**
       Summary: \[ [ mk_bvmul_no_underflow c t1 t2 ] \]
       Create a predicate that checks that the bit-wise signed multiplication
       of [t1] and [t2] does not underflow.
       
       The nodes [t1] and [t2] must have the same bit-vector sort.
*)
val mk_bvmul_no_underflow : context -> ast -> ast -> ast
	

(**
       {2 {L Arrays}}
*)
(**
       Summary: \[ [ mk_select c a i ] \]
       Array read.
       The argument [a] is the array and [i] is the index of the array that gets read.      
 
       The node [a] must have an array sort {e [domain -> range] }, 
       and [i] must have the sort [domain].
       The sort of the result is [range].

       - {b See also}: {!mk_array_sort}
       - {b See also}: {!mk_store}
*)
val mk_select : context -> ast -> ast -> ast
	

(**
       Summary: \[ [ mk_store c a i v ] \]
       Array update.
       
       The node [a] must have an array sort {e [domain -> range] }, [i] must have sort [domain],
       [v] must have sort range. The sort of the result is {e [domain -> range] }.
       The semantics of this function is given by the theory of arrays described in the SMT-LIB
       standard. See http:
       The result of this function is an array that is equal to [a] (with respect to [select)]
       on all indices except for [i], where it maps to [v] (and the [select] of [a] with 
       respect to [i] may be a different value).
       
       - {b See also}: {!mk_array_sort}
       - {b See also}: {!mk_select}
*)
val mk_store : context -> ast -> ast -> ast -> ast
	

(**
        Summary: Create the constant array.
         
        The resulting term is an array, such that a [select] on an arbitrary index 
        produces the value [v].

        @param c logical context.
        @param domain domain sort for the array.
        @param v value that the array maps to.
*)
val mk_const_array : context -> sort -> ast -> ast
	

(**
       Summary: \[ [ mk_map f n args ] \]
       map f on the the argument arrays.
       
       The [n] nodes [args] must be of array sorts {e [domain_i -> range_i] }.
       The function declaration [f] must have type {e  range_1 .. range_n -> range }.
       [v] must have sort range. The sort of the result is {e [domain_i -> range] }.
       
       - {b See also}: {!mk_array_sort}
       - {b See also}: {!mk_store}
       - {b See also}: {!mk_select}
*)
val mk_map : context -> func_decl -> int -> ast -> ast
	

(**
        Summary: Access the array default value.
        Produces the default range value, for arrays that can be represented as 
        finite maps with a default range value.

        @param c logical context.
        @param array array value whose default range value is accessed.

*)
val mk_array_default : context -> ast -> ast
	

(**
       {2 {L Sets}}
*)
(**
       Summary: Create Set type.
*)
val mk_set_sort : context -> sort -> sort
	

(**
        Summary: Create the empty set.
*)
val mk_empty_set : context -> sort -> ast
	

(**
        Summary: Create the full set.
*)
val mk_full_set : context -> sort -> ast
	

(**
       Summary: Add an element to a set.
       
       The first argument must be a set, the second an element.
*)
val mk_set_add : context -> ast -> ast -> ast
	

(**
       Summary: Remove an element to a set.
       
       The first argument must be a set, the second an element.
*)
val mk_set_del : context -> ast -> ast -> ast
	

(**
       Summary: Take the union of a list of sets.
*)
val mk_set_union : context -> ast array -> ast
	

(**
       Summary: Take the intersection of a list of sets.
*)
val mk_set_intersect : context -> ast array -> ast
	

(**
       Summary: Take the set difference between two sets.
*)
val mk_set_difference : context -> ast -> ast -> ast
	

(**
       Summary: Take the complement of a set.
*)
val mk_set_complement : context -> ast -> ast
	

(**
       Summary: Check for set membership.
       
       The first argument should be an element type of the set.
*)
val mk_set_member : context -> ast -> ast -> ast
	

(**
       Summary: Check for subsetness of sets.
*)
val mk_set_subset : context -> ast -> ast -> ast
	

(**
       {2 {L Numerals}}
*)
(**
        {4 {L Redundant low-level API}} 
*)
(**
       Summary: Create a numeral of a given sort. 

       @param c logical context.
       @param numeral A string representing the numeral value in decimal notation. If the given sort is a real, then the numeral can be a rational, that is, a string of the form {e [num]* / [num]* }.
       @param ty The sort of the numeral. In the current implementation, the given sort can be an int, real, finite-domain, or bit-vectors of arbitrary size. 
       
       - {b See also}: {!mk_int}
       
*)
val mk_numeral : context -> string -> sort -> ast
	

(**
       Summary: Create a real from a fraction.

       @param c logical context.
       @param num numerator of rational.
       @param den denomerator of rational.

       - {b Precondition}: den != 0

       - {b See also}: {!mk_numeral}
       - {b See also}: {!mk_int}
       
*)
val mk_real : context -> int -> int -> ast
	

(**
       Summary: Create a numeral of an int, bit-vector, or finite-domain sort. 
       
       This function can be use to create numerals that fit in a machine integer.
       It is slightly faster than {!mk_numeral} since it is not necessary to parse a string.

       - {b See also}: {!mk_numeral}
*)
val mk_int : context -> int -> sort -> ast
	

(**
       Summary: Create a numeral of a int, bit-vector, or finite-domain sort. 
       
       This function can be use to create numerals that fit in a machine long long integer.
       It is slightly faster than {!mk_numeral} since it is not necessary to parse a string.

       - {b See also}: {!mk_numeral}
*)
val mk_int64 : context -> int64 -> sort -> ast
	

(**
       {2 {L Quantifiers}}
*)
(**
       Summary: Create a pattern for quantifier instantiation.

       Z3 uses pattern matching to instantiate quantifiers. If a
       pattern is not provided for a quantifier, then Z3 will
       automatically compute a set of patterns for it. However, for
       optimal performance, the user should provide the patterns.

       Patterns comprise a list of terms. The list should be
       non-empty.  If the list comprises of more than one term, it is
       a called a multi-pattern.
       
       In general, one can pass in a list of (multi-)patterns in the
       quantifier constructor.


       - {b See also}: {!mk_forall}
       - {b See also}: {!mk_exists}
*)
val mk_pattern : context -> ast array -> pattern
	

(**
       Summary: Create a bound variable.

       Bound variables are indexed by de-Bruijn indices. It is perhaps easiest to explain
       the meaning of de-Bruijn indices by indicating the compilation process from
       non-de-Bruijn formulas to de-Bruijn format.

       {v  
       abs(forall (x1) phi) = forall (x1) abs1(phi, x1, 0)
       abs(forall (x1, x2) phi) = abs(forall (x1) abs(forall (x2) phi))
       abs1(x, x, n) = b_n
       abs1(y, x, n) = y
       abs1(f(t1,...,tn), x, n) = f(abs1(t1,x,n), ..., abs1(tn,x,n))
       abs1(forall (x1) phi, x, n) = forall (x1) (abs1(phi, x, n+1))
        v}

       The last line is significant: the index of a bound variable is different depending
       on the scope in which it appears. The deeper x appears, the higher is its
       index.
       
       @param c logical context
       @param index de-Bruijn index
       @param ty sort of the bound variable

       - {b See also}: {!mk_forall}
       - {b See also}: {!mk_exists}
*)
val mk_bound : context -> int -> sort -> ast
	

(**
       Summary: Create a forall formula. It takes an expression [body] that contains bound variables
       of the same sorts as the sorts listed in the array [sorts]. The bound variables are de-Bruijn indices created
       using {!mk_bound}. The array [decl_names] contains the names that the quantified formula uses for the 
       bound variables. Z3 applies the convention that the last element in the [decl_names] and [sorts] array
       refers to the variable with index 0, the second to last element of [decl_names] and [sorts] refers
       to the variable with index 1, etc.
       

        [mk_forall c w p t n b] creates a forall formula, where
       [w] is the weight, [p] is an array of patterns, [t] is an array
       with the sorts of the bound variables, [n] is an array with the
       'names' of the bound variables, and [b] is the body of the
       quantifier. Quantifiers are associated with weights indicating
       the importance of using the quantifier during
       instantiation. 
       
       
       @param c logical context.
       @param weight quantifiers are associated with weights indicating the importance of using the quantifier during instantiation. By default, pass the weight 0.
       @param num_patterns number of patterns.
       @param patterns array containing the patterns created using {!mk_pattern}.
       @param num_decls number of variables to be bound.
       @param sorts the sorts of the bound variables.
       @param decl_names names of the bound variables
       @param body the body of the quantifier.
       
       - {b See also}: {!mk_pattern}
       - {b See also}: {!mk_bound}
       - {b See also}: {!mk_exists}
*)
val mk_forall : context -> int -> pattern array -> sort array -> symbol array -> ast -> ast
	

(**
       Summary: Create an exists formula. Similar to {!mk_forall}.
       
       - {b See also}: {!mk_pattern}
       - {b See also}: {!mk_bound}
       - {b See also}: {!mk_forall}
       - {b See also}: {!mk_quantifier}
*)
val mk_exists : context -> int -> pattern array -> sort array -> symbol array -> ast -> ast
	

(**
       Summary: Create a quantifier - universal or existential, with pattern hints. 
       See the documentation for {!mk_forall} for an explanation of the parameters.
       
       @param c logical context.
       @param is_forall flag to indicate if this is a universal or existential quantifier.
       @param weight quantifiers are associated with weights indicating the importance of using the quantifier during instantiation. By default, pass the weight 0.
       @param num_patterns number of patterns.
       @param patterns array containing the patterns created using {!mk_pattern}.
       @param num_decls number of variables to be bound.
       @param sorts array of sorts of the bound variables.
       @param decl_names names of the bound variables.
       @param body the body of the quantifier.
       
       - {b See also}: {!mk_pattern}
       - {b See also}: {!mk_bound}
       - {b See also}: {!mk_forall}
       - {b See also}: {!mk_exists}
*)
val mk_quantifier : context -> bool -> int -> pattern array -> sort array -> symbol array -> ast -> ast
	

(**
       Summary: Create a quantifier - universal or existential, with pattern hints, no patterns, and attributes
       
       @param c logical context.
       @param is_forall flag to indicate if this is a universal or existential quantifier.
       @param quantifier_id identifier to identify quantifier
       @param skolem_id identifier to identify skolem constants introduced by quantifier.
       @param weight quantifiers are associated with weights indicating the importance of using the quantifier during instantiation. By default, pass the weight 0.
       @param num_patterns number of patterns.
       @param patterns array containing the patterns created using {!mk_pattern}.
       @param num_no_patterns number of patterns.
       @param no_patterns array containing the patterns created using {!mk_pattern}.
       @param num_decls number of variables to be bound.
       @param sorts array of sorts of the bound variables.
       @param decl_names names of the bound variables.
       @param body the body of the quantifier.
       
       - {b See also}: {!mk_pattern}
       - {b See also}: {!mk_bound}
       - {b See also}: {!mk_forall}
       - {b See also}: {!mk_exists}
*)
val mk_quantifier_ex : context -> bool -> int -> symbol -> symbol -> pattern array -> ast array -> sort array -> symbol array -> ast -> ast
	

(**
       Summary: Create a universal quantifier using a list of constants that
       will form the set of bound variables.

       @param c logical context.
       @param weight quantifiers are associated with weights indicating the importance of using 
              the quantifier during instantiation. By default, pass the weight 0.
       @param num_bound number of constants to be abstracted into bound variables.
       @param bound array of constants to be abstracted into bound variables.
       @param num_patterns number of patterns.
       @param patterns array containing the patterns created using {!mk_pattern}.
       @param body the body of the quantifier.
       
       - {b See also}: {!mk_pattern}
       - {b See also}: {!mk_exists_const}

*)
val mk_forall_const : context -> int -> app array -> pattern array -> ast -> ast
	

(**
       Summary: Similar to {!mk_forall_const}.

       Summary: Create an existential quantifier using a list of constants that
       will form the set of bound variables.

       @param c logical context.
       @param weight quantifiers are associated with weights indicating the importance of using 
              the quantifier during instantiation. By default, pass the weight 0.
       @param num_bound number of constants to be abstracted into bound variables.
       @param bound array of constants to be abstracted into bound variables.
       @param num_patterns number of patterns.
       @param patterns array containing the patterns created using {!mk_pattern}.
       @param body the body of the quantifier.
       
       - {b See also}: {!mk_pattern}
       - {b See also}: {!mk_forall_const}
*)
val mk_exists_const : context -> int -> app array -> pattern array -> ast -> ast
	

(**
       Summary: Create a universal or existential 
       quantifier using a list of constants that
       will form the set of bound variables.
*)
val mk_quantifier_const : context -> bool -> int -> app array -> pattern array -> ast -> ast
	

(**
       Summary: Create a universal or existential 
       quantifier using a list of constants that
       will form the set of bound variables.
*)
val mk_quantifier_const_ex : context -> bool -> int -> symbol -> symbol -> app array -> pattern array -> ast array -> ast -> ast
	

(**
       {2 {L Accessors}}
*)
(**
        {3 {L Symbols}} 
*)
(**
        {4 {L Redundant low-level API}} 
*)
(**
       Summary: Return [INT_SYMBOL] if the symbol was constructed
       using {!mk_int_symbol}, and [STRING_SYMBOL] if the symbol
       was constructed using {!mk_string_symbol}.
*)
val get_symbol_kind : context -> symbol -> symbol_kind
	

(**
       Summary: \[ [ get_symbol_int c s ] \]
       Return the symbol int value. 
       
       - {b Precondition}: get_symbol_kind s == INT_SYMBOL

       - {b See also}: {!mk_int_symbol}
*)
val get_symbol_int : context -> symbol -> int
	

(**
       Summary: \[ [ get_symbol_string c s ] \]
       Return the symbol name. 

       - {b Precondition}: get_symbol_string s == STRING_SYMBOL

       
       
       

       - {b See also}: {!mk_string_symbol}
*)
val get_symbol_string : context -> symbol -> string
	

(**
        {3 {L Sorts}} 
*)
(**
       Summary: Return the sort name as a symbol. 
*)
val get_sort_name : context -> sort -> symbol
	

(**
        Summary: Return a unique identifier for [s].
         - {b Remarks}: Implicitly used by [Pervasives.( = )] and [Pervasives.compare]. 
*)
val get_sort_id : context -> sort -> int
	

(**
        {4 {L Redundant low-level API}} 
*)
(**
       Summary: Convert a [sort] into [ast]. 
        - {b Remarks}: [sort_to_ast c s] can be replaced by [(s :> ast)]. 
*)
val sort_to_ast : context -> sort -> ast
	

(**
       Summary: compare sorts.
        - {b Remarks}: [Pervasives.( = )] or [Pervasives.compare] can also be used. 
*)
val is_eq_sort : context -> sort -> sort -> bool
	

(**
       Summary: Return the sort kind (e.g., array, tuple, int, bool, etc).

       - {b See also}: {!sort_kind}
*)
val get_sort_kind : context -> sort -> sort_kind
	

(**
       Summary: \[ [ get_bv_sort_size c t ] \]
       Return the size of the given bit-vector sort. 

       - {b Precondition}: get_sort_kind c t == BV_SORT

       - {b See also}: {!mk_bv_sort}
       - {b See also}: {!get_sort_kind}
*)
val get_bv_sort_size : context -> sort -> int
	

(**
        
         Summary: Return the size of the sort in [r].  Return [None] if the call failed. 
        That is, get_sort_kind(s) == FINITE_DOMAIN_SORT
*)
val get_finite_domain_sort_size : context -> sort -> int64 option
	

(**
       Summary: \[ [ get_array_sort_domain c t ] \]
       Return the domain of the given array sort.
       
       - {b Precondition}: get_sort_kind c t == ARRAY_SORT

       - {b See also}: {!mk_array_sort}
       - {b See also}: {!get_sort_kind}
*)
val get_array_sort_domain : context -> sort -> sort
	

(**
       Summary: \[ [ get_array_sort_range c t ] \] 
       Return the range of the given array sort. 

       - {b Precondition}: get_sort_kind c t == ARRAY_SORT

       - {b See also}: {!mk_array_sort}
       - {b See also}: {!get_sort_kind}
*)
val get_array_sort_range : context -> sort -> sort
	

(**
       Summary: \[ [ get_tuple_sort_mk_decl c t ] \]
       Return the constructor declaration of the given tuple
       sort. 

       - {b Precondition}: get_sort_kind c t == DATATYPE_SORT

       - {b See also}: {!mk_tuple_sort}
       - {b See also}: {!get_sort_kind}
*)
val get_tuple_sort_mk_decl : context -> sort -> func_decl
	

(**
       Summary: \[ [ get_tuple_sort_num_fields c t ] \]
       Return the number of fields of the given tuple sort. 

       - {b Precondition}: get_sort_kind c t == DATATYPE_SORT

       - {b See also}: {!mk_tuple_sort}
       - {b See also}: {!get_sort_kind}
*)
val get_tuple_sort_num_fields : context -> sort -> int
	

(**
       Summary: \[ [ get_tuple_sort_field_decl c t i ] \]
       Return the i-th field declaration (i.e., projection function declaration)
       of the given tuple sort. 

       - {b Precondition}: get_sort_kind t == DATATYPE_SORT
       - {b Precondition}: i < get_tuple_sort_num_fields c t
       
       - {b See also}: {!mk_tuple_sort}
       - {b See also}: {!get_sort_kind}
*)
val get_tuple_sort_field_decl : context -> sort -> int -> func_decl
	

(**
        Summary: Return number of constructors for datatype.

        - {b Precondition}: get_sort_kind t == DATATYPE_SORT

        - {b See also}: {!get_datatype_sort_constructor}
        - {b See also}: {!get_datatype_sort_recognizer}
        - {b See also}: {!get_datatype_sort_constructor_accessor}

*)
val get_datatype_sort_num_constructors : context -> sort -> int
	

(**
        Summary: Return idx'th constructor.

        - {b Precondition}: get_sort_kind t == DATATYPE_SORT
        - {b Precondition}: idx < get_datatype_sort_num_constructors c t

        - {b See also}: {!get_datatype_sort_num_constructors}
        - {b See also}: {!get_datatype_sort_recognizer}
        - {b See also}: {!get_datatype_sort_constructor_accessor}

*)
val get_datatype_sort_constructor : context -> sort -> int -> func_decl
	

(**
        Summary: Return idx'th recognizer.

        - {b Precondition}: get_sort_kind t == DATATYPE_SORT
        - {b Precondition}: idx < get_datatype_sort_num_constructors c t

        - {b See also}: {!get_datatype_sort_num_constructors}
        - {b See also}: {!get_datatype_sort_constructor}
        - {b See also}: {!get_datatype_sort_constructor_accessor}

*)
val get_datatype_sort_recognizer : context -> sort -> int -> func_decl
	

(**
        Summary: Return idx_a'th accessor for the idx_c'th constructor.

        - {b Precondition}: get_sort_kind t == DATATYPE_SORT
        - {b Precondition}: idx_c < get_datatype_sort_num_constructors c t
        - {b Precondition}: idx_a < get_domain_size c get_datatype_sort_constructor c idx_c

        - {b See also}: {!get_datatype_sort_num_constructors}
        - {b See also}: {!get_datatype_sort_constructor}
        - {b See also}: {!get_datatype_sort_recognizer}
*)
val get_datatype_sort_constructor_accessor : context -> sort -> int -> int -> func_decl
	

(**
        Summary: Return arity of relation.

        - {b Precondition}: get_sort_kind s == RELATION_SORT

        - {b See also}: {!get_relation_column}
*)
val get_relation_arity : context -> sort -> int
	

(**
        Summary: Return sort at i'th column of relation sort.

        - {b Precondition}: get_sort_kind c s == RELATION_SORT
        - {b Precondition}: col < get_relation_arity c s

        - {b See also}: {!get_relation_arity}
*)
val get_relation_column : context -> sort -> int -> sort
	

(**
        {3 {L Function Declarations}} 
*)
(**
       Summary: Convert a [func_decl] into [ast]. 
        - {b Remarks}: [func_decl_to_ast c f]  can be replaced by [(f :> ast)]. 
*)
val func_decl_to_ast : context -> func_decl -> ast
	

(**
       Summary: compare terms.
        - {b Remarks}: [Pervasives.( = )] or [Pervasives.compare] can also be used. 
*)
val is_eq_func_decl : context -> func_decl -> func_decl -> bool
	

(**
        Summary: Return a unique identifier for [f].
         - {b Remarks}: Implicitly used by [Pervasives.( = )] and [Pervasives.compare]. 
*)
val get_func_decl_id : context -> func_decl -> int
	

(**
       Summary: Return the constant declaration name as a symbol. 
*)
val get_decl_name : context -> func_decl -> symbol
	

(**
       Summary: Return declaration kind corresponding to declaration.
*)
val get_decl_kind : context -> func_decl -> decl_kind
	

(**
       Summary: Return the number of parameters of the given declaration.

       - {b See also}: {!get_arity}
*)
val get_domain_size : context -> func_decl -> int
	

(**
       Summary: Alias for [get_domain_size].

       - {b See also}: {!get_domain_size}
*)
val get_arity : context -> func_decl -> int
	

(**
       Summary: \[ [ get_domain c d i ] \]
       Return the sort of the i-th parameter of the given function declaration.
       
       - {b Precondition}: i < get_domain_size d

       - {b See also}: {!get_domain_size}
*)
val get_domain : context -> func_decl -> int -> sort
	

(**
       Summary: \[ [ get_range c d ] \]
       Return the range of the given declaration. 

       If [d] is a constant (i.e., has zero arguments), then this
       function returns the sort of the constant.
*)
val get_range : context -> func_decl -> sort
	

(**
       Summary: Return the number of parameters associated with a declaration.
*)
val get_decl_num_parameters : context -> func_decl -> int
	

(**
       Summary: Return the parameter type associated with a declaration.
       
       @param c the context
       @param d the function declaration
       @param idx is the index of the named parameter it should be between 0 and the number of parameters.
*)
val get_decl_parameter_kind : context -> func_decl -> int -> parameter_kind
	

(**
       Summary: Return the integer value associated with an integer parameter.

       - {b Precondition}: get_decl_parameter_kind c d idx == PARAMETER_INT
*)
val get_decl_int_parameter : context -> func_decl -> int -> int
	

(**
       Summary: Return the double value associated with an double parameter.

       - {b Precondition}: get_decl_parameter_kind c d idx == PARAMETER_DOUBLE
*)
val get_decl_double_parameter : context -> func_decl -> int -> float
	

(**
       Summary: Return the double value associated with an double parameter.

       - {b Precondition}: get_decl_parameter_kind c d idx == PARAMETER_SYMBOL
*)
val get_decl_symbol_parameter : context -> func_decl -> int -> symbol
	

(**
       Summary: Return the sort value associated with a sort parameter.

       - {b Precondition}: get_decl_parameter_kind c d idx == PARAMETER_SORT
*)
val get_decl_sort_parameter : context -> func_decl -> int -> sort
	

(**
       Summary: Return the expresson value associated with an expression parameter.

       - {b Precondition}: get_decl_parameter_kind c d idx == PARAMETER_AST
*)
val get_decl_ast_parameter : context -> func_decl -> int -> ast
	

(**
       Summary: Return the expresson value associated with an expression parameter.

       - {b Precondition}: get_decl_parameter_kind c d idx == PARAMETER_FUNC_DECL
*)
val get_decl_func_decl_parameter : context -> func_decl -> int -> func_decl
	

(**
       Summary: Return the rational value, as a string, associated with a rational parameter.

       - {b Precondition}: get_decl_parameter_kind c d idx == PARAMETER_RATIONAL
*)
val get_decl_rational_parameter : context -> func_decl -> int -> string
	

(**
        {3 {L Applications}} 
*)
(**
       Summary: Convert a [app] into [ast]. 
        - {b Remarks}: [app_to_ast c a] can be replaced by [(a :> ast)]. 
*)
val app_to_ast : context -> app -> ast
	

(**
       Summary: Return the declaration of a constant or function application.
*)
val get_app_decl : context -> app -> func_decl
	

(**
       Summary: \[ [ get_app_num_args c a ] \]
       Return the number of argument of an application. If [t]
       is an constant, then the number of arguments is 0.
*)
val get_app_num_args : context -> app -> int
	

(**
       Summary: \[ [ get_app_arg c a i ] \]
       Return the i-th argument of the given application.
       
       - {b Precondition}: i < get_num_args c a
*)
val get_app_arg : context -> app -> int -> ast
	

(**
        {3 {L Terms}} 
*)
(**
       Summary: compare terms.
        - {b Remarks}: [Pervasives.( = )] or [Pervasives.compare] can also be used. 
*)
val is_eq_ast : context -> ast -> ast -> bool
	

(**
        Summary: Return a unique identifier for [t].
         - {b Remarks}: Implicitly used by [Pervasives.compare] for values of type [ast], [app], [sort], [func_decl], and [pattern]. 
*)
val get_ast_id : context -> ast -> int
	

(**
       Summary: Return a hash code for the given AST.
        - {b Remarks}: Implicitly used by [Hashtbl.hash] for values of type [ast], [app], [sort], [func_decl], and [pattern]. 
*)
val get_ast_hash : context -> ast -> int
	

(**
       Summary: Return the sort of an AST node.
       
       The AST node must be a constant, application, numeral, bound variable, or quantifier.

*)
val get_sort : context -> ast -> sort
	

(**
       Summary: Return true if the given expression [t] is well sorted.
*)
val is_well_sorted : context -> ast -> bool
	

(**
       Summary: Return L_TRUE if [a] is true, L_FALSE if it is false, and L_UNDEF otherwise.
*)
val get_bool_value : context -> ast -> lbool
	

(**
       Summary: Return the kind of the given AST.
*)
val get_ast_kind : context -> ast -> ast_kind
	

val is_app : context -> ast -> bool
	

val is_numeral_ast : context -> ast -> bool
	

(**
       Summary: Return true if the give AST is a real algebraic number.
*)
val is_algebraic_number : context -> ast -> bool
	

(**
       Summary: Convert an [ast] into an [APP_AST]. 
       
       - {b Precondition}: {v  get_ast_kind c a == [APP_AST]  v}
*)
val to_app : context -> ast -> app
	

(**
       Summary: Convert an AST into a FUNC_DECL_AST. This is just type casting.
       
       - {b Precondition}: {v  get_ast_kind c a == FUNC_DECL_AST  v}
*)
val to_func_decl : context -> ast -> func_decl
	

(**
        {4 {L Numerals}} 
*)
(**
        {5 {L Low-level API}} 
*)
(**
       Summary: Return numeral value, as a string of a numeric constant term

       - {b Precondition}: get_ast_kind c a == NUMERAL_AST
*)
val get_numeral_string : context -> ast -> string
	

(**
       Summary: Return numeral as a string in decimal notation.
       The result has at most [precision] decimal places.

       - {b Precondition}: get_ast_kind c a == NUMERAL_AST || is_algebraic_number c a
*)
val get_numeral_decimal_string : context -> ast -> int -> string
	

(**
       Summary: Return the numerator (as a numeral AST) of a numeral AST of sort Real.

       - {b Precondition}: get_ast_kind c a == NUMERAL_AST
*)
val get_numerator : context -> ast -> ast
	

(**
       Summary: Return the denominator (as a numeral AST) of a numeral AST of sort Real.

       - {b Precondition}: get_ast_kind c a == NUMERAL_AST
*)
val get_denominator : context -> ast -> ast
	

(**
       Summary: Return numeral value, as a pair of 64 bit numbers if the representation fits.

       @param c logical context.
       @param a term.
       @param num numerator.
       @param den denominator.
       
       Return [TRUE] if the numeral value fits in 64 bit numerals, [FALSE] otherwise.

       - {b Precondition}: get_ast_kind a == NUMERAL_AST
*)
val get_numeral_small : context -> ast -> bool * int64 * int64
	

(**
       Summary: \[ [ get_numeral_int c v ] \]
       Similar to {!get_numeral_string}, but only succeeds if
       the value can fit in a machine int. Return TRUE if the call succeeded.

       - {b Precondition}: get_ast_kind c v == NUMERAL_AST
      
       - {b See also}: {!get_numeral_string}
*)
val get_numeral_int : context -> ast -> bool * int
	

(**
       Summary: \[ [ get_numeral_int64 c v ] \]
       Similar to {!get_numeral_string}, but only succeeds if
       the value can fit in a machine long long int. Return TRUE if the call succeeded.

       - {b Precondition}: get_ast_kind c v == NUMERAL_AST

       - {b See also}: {!get_numeral_string}
*)
val get_numeral_int64 : context -> ast -> bool * int64
	

(**
       Summary: \[ [ get_numeral_rational_int64 c x y] \]
       Similar to {!get_numeral_string}, but only succeeds if
       the value can fit as a rational number as machine long long int. Return TRUE if the call succeeded.

       - {b Precondition}: get_ast_kind c v == NUMERAL_AST

       - {b See also}: {!get_numeral_string}
*)
val get_numeral_rational_int64 : context -> ast -> bool * int64 * int64
	

(**
       Summary: Return a lower bound for the given real algebraic number. 
       The interval isolating the number is smaller than 1/10^precision.
       The result is a numeral AST of sort Real.

       - {b Precondition}: is_algebraic_number c a
*)
val get_algebraic_number_lower : context -> ast -> int -> ast
	

(**
       Summary: Return a upper bound for the given real algebraic number. 
       The interval isolating the number is smaller than 1/10^precision.
       The result is a numeral AST of sort Real.

       - {b Precondition}: is_algebraic_number c a
*)
val get_algebraic_number_upper : context -> ast -> int -> ast
	

(**
        {4 {L Patterns}} 
*)
(**
       Summary: Convert a pattern into ast. 
        - {b Remarks}: [pattern_to_ast c p]  can be replaced by [(p :> ast)]. 
*)
val pattern_to_ast : context -> pattern -> ast
	

(**
        Summary: Return number of terms in pattern.
*)
val get_pattern_num_terms : context -> pattern -> int
	

(**
       Summary: Return i'th ast in pattern.
*)
val get_pattern : context -> pattern -> int -> ast
	

(**
        {4 {L Quantifiers}} 
*)
(**
       Summary: Return index of de-Brujin bound variable.

       - {b Precondition}: get_ast_kind a == VAR_AST
*)
val get_index_value : context -> ast -> int
	

(**
       Summary: Determine if quantifier is universal.
       
       - {b Precondition}: get_ast_kind a == QUANTIFIER_AST
*)
val is_quantifier_forall : context -> ast -> bool
	

(**
       Summary: Obtain weight of quantifier.
       
       - {b Precondition}: get_ast_kind a == QUANTIFIER_AST
*)
val get_quantifier_weight : context -> ast -> int
	

(**
       Summary: Return number of patterns used in quantifier.
       
       - {b Precondition}: get_ast_kind a == QUANTIFIER_AST
*)
val get_quantifier_num_patterns : context -> ast -> int
	

(**
       Summary: Return i'th pattern.
       
       - {b Precondition}: get_ast_kind a == QUANTIFIER_AST
*)
val get_quantifier_pattern_ast : context -> ast -> int -> pattern
	

(**
       Summary: Return number of no_patterns used in quantifier.
       
       - {b Precondition}: get_ast_kind a == QUANTIFIER_AST
*)
val get_quantifier_num_no_patterns : context -> ast -> int
	

(**
       Summary: Return i'th no_pattern.
       
       - {b Precondition}: get_ast_kind a == QUANTIFIER_AST
*)
val get_quantifier_no_pattern_ast : context -> ast -> int -> ast
	

(**
       Summary: Return symbol of the i'th bound variable.
       
       - {b Precondition}: get_ast_kind a == QUANTIFIER_AST
*)
val get_quantifier_bound_name : context -> ast -> int -> symbol
	

(**
       Summary: Return sort of the i'th bound variable.
       
       - {b Precondition}: get_ast_kind a == QUANTIFIER_AST
*)
val get_quantifier_bound_sort : context -> ast -> int -> sort
	

(**
       Summary: Return body of quantifier.
       
       - {b Precondition}: get_ast_kind a == QUANTIFIER_AST
*)
val get_quantifier_body : context -> ast -> ast
	

(**
       Summary: Return number of bound variables of quantifier.
       
       - {b Precondition}: get_ast_kind a == QUANTIFIER_AST
*)
val get_quantifier_num_bound : context -> ast -> int
	

(**
        {3 {L Simplification}} 
*)
(**
        Summary: Interface to simplifier.

        Provides an interface to the AST simplifier used by Z3.
*)
val simplify : context -> ast -> ast
	

(**
       {2 {L Modifiers}}
*)
(**
       Summary: Update the arguments of term [a] using the arguments [args].
       The number of arguments [num_args] should coincide 
       with the number of arguments to [a].
       If [a] is a quantifier, then num_args has to be 1.
*)
val update_term : context -> ast -> ast array -> ast
	

(**
       Summary: Substitute every occurrence of {e from[i] } in [a] with {e to[i] }, for [i] smaller than [num_exprs].
       The result is the new AST. The arrays [from] and [to] must have size [num_exprs].
       For every [i] smaller than [num_exprs], we must have that sort of {e from[i] } must be equal to sort of {e to[i] }.
*)
val substitute : context -> ast -> ast array -> ast array -> ast
	

(**
       Summary: Substitute the free variables in [a] with the expressions in [to].
       For every [i] smaller than [num_exprs], the variable with de-Bruijn index [i] is replaced with term {e to[i] }.
*)
val substitute_vars : context -> ast -> ast array -> ast
	

(**
       {2 {L Interaction logging.}}
*)
(**
       Summary: Log interaction to a file.
*)
val open_log : string -> bool
	

(**
       Summary: Append user-defined string to interaction log.
       
       The interaction log is opened using open_log.
       It contains the formulas that are checked using Z3.
       You can use this command to append comments, for instance.
*)
val append_log : string -> unit
	

(**
       Summary: Close interaction log.
*)
val close_log : unit -> unit
	

(**
       Summary: Enable/disable printing warning messages to the console.

       Warnings are printed after passing [true], warning messages are
       suppressed after calling this method with [false].       
*)
val toggle_warning_messages : bool -> unit
	

(**
       {2 {L String conversion}}
*)
(**
       Summary: Select mode for the format used for pretty-printing AST nodes.

       The default mode for pretty printing AST nodes is to produce
       SMT-LIB style output where common subexpressions are printed 
       at each occurrence. The mode is called PRINT_SMTLIB_FULL.
       To print shared common subexpressions only once, 
       use the PRINT_LOW_LEVEL mode.
       To print in way that conforms to SMT-LIB standards and uses let
       expressions to share common sub-expressions use PRINT_SMTLIB_COMPLIANT.

       - {b See also}: {!ast_to_string}
       - {b See also}: {!pattern_to_string}
       - {b See also}: {!func_decl_to_string}

*)
val set_ast_print_mode : context -> ast_print_mode -> unit
	

(**
       Summary: Convert the given AST node into a string.

       
       
       
       - {b See also}: {!pattern_to_string}
       - {b See also}: {!sort_to_string}
*)
val ast_to_string : context -> ast -> string
	

val pattern_to_string : context -> pattern -> string
	

val sort_to_string : context -> sort -> string
	

val func_decl_to_string : context -> func_decl -> string
	

(**
       Summary: Convert the given model into a string.

       
       
       
*)
val model_to_string : context -> model -> string
	

(**
       Summary: Convert the given benchmark into SMT-LIB formatted string.

       
       
       

       @param c - context.
       @param name - name of benchmark. The argument is optional.
       @param logic - the benchmark logic. 
       @param status - the status string (sat, unsat, or unknown)
       @param attributes - other attributes, such as source, difficulty or category.
       @param num_assumptions - number of assumptions.
       @param assumptions - auxiliary assumptions.
       @param formula - formula to be checked for consistency in conjunction with assumptions.
*)
val benchmark_to_smtlib_string : context -> string -> string -> string -> string -> ast array -> ast -> string
	

(**
       {2 {L Parser interface}}
*)
(**
       Summary: \[ [ parse_smtlib2_string c str ] \]
       Parse the given string using the SMT-LIB2 parser. 
              
       It returns a formula comprising of the conjunction of assertions in the scope
       (up to push/pop) at the end of the string.
*)
val parse_smtlib2_string : context -> string -> symbol array -> sort array -> symbol array -> func_decl array -> ast
	

(**
       Summary: Similar to {!parse_smtlib2_string}, but reads the benchmark from a file.
*)
val parse_smtlib2_file : context -> string -> symbol array -> sort array -> symbol array -> func_decl array -> ast
	

(**
        {4 {L Low-level API}} 
*)
(**
       Summary: \[ [ parse_smtlib_string c str sort_names sorts decl_names decls ] \]
       Parse the given string using the SMT-LIB parser. 
              
       The symbol table of the parser can be initialized using the given sorts and declarations. 
       The symbols in the arrays [sort_names] and [decl_names] don't need to match the names
       of the sorts and declarations in the arrays [sorts] and [decls]. This is an useful feature
       since we can use arbitrary names to reference sorts and declarations defined using the C API.

       The formulas, assumptions and declarations defined in [str] can be extracted using the functions:
       {!get_smtlib_num_formulas}, {!get_smtlib_formula}, {!get_smtlib_num_assumptions}, {!get_smtlib_assumption}, 
       {!get_smtlib_num_decls}, and {!get_smtlib_decl}.
*)
val parse_smtlib_string : context -> string -> symbol array -> sort array -> symbol array -> func_decl array -> unit
	

(**
       Summary: Similar to {!parse_smtlib_string}, but reads the benchmark from a file.
*)
val parse_smtlib_file : context -> string -> symbol array -> sort array -> symbol array -> func_decl array -> unit
	

(**
       Summary: Return the number of SMTLIB formulas parsed by the last call to {!parse_smtlib_string} or {!parse_smtlib_file}.
*)
val get_smtlib_num_formulas : context -> int
	

(**
       Summary: \[ [ get_smtlib_formula c i ] \]
       Return the i-th formula parsed by the last call to {!parse_smtlib_string} or {!parse_smtlib_file}.

       - {b Precondition}: i < get_smtlib_num_formulas c
*)
val get_smtlib_formula : context -> int -> ast
	

(**
       Summary: Return the number of SMTLIB assumptions parsed by {!parse_smtlib_string} or {!parse_smtlib_file}.
*)
val get_smtlib_num_assumptions : context -> int
	

(**
       Summary: \[ [ get_smtlib_assumption c i ] \]
       Return the i-th assumption parsed by the last call to {!parse_smtlib_string} or {!parse_smtlib_file}.

       - {b Precondition}: i < get_smtlib_num_assumptions c
*)
val get_smtlib_assumption : context -> int -> ast
	

(**
       Summary: Return the number of declarations parsed by {!parse_smtlib_string} or {!parse_smtlib_file}.
*)
val get_smtlib_num_decls : context -> int
	

(**
       Summary: \[ [ get_smtlib_decl c i ] \]
       Return the i-th declaration parsed by the last call to {!parse_smtlib_string} or {!parse_smtlib_file}.

       - {b Precondition}: i < get_smtlib_num_decls c
*)
val get_smtlib_decl : context -> int -> func_decl
	

(**
       Summary: Return the number of sorts parsed by {!parse_smtlib_string} or {!parse_smtlib_file}.
*)
val get_smtlib_num_sorts : context -> int
	

(**
       Summary: \[ [ get_smtlib_sort c i ] \]
       Return the i-th sort parsed by the last call to {!parse_smtlib_string} or {!parse_smtlib_file}.

       - {b Precondition}: i < get_smtlib_num_sorts c
*)
val get_smtlib_sort : context -> int -> sort
	

(**
       Summary: \[ [ get_smtlib_error c ] \]
       Retrieve that last error message information generated from parsing.
*)
val get_smtlib_error : context -> string
	

(**
       Summary: \[ [ parse_z3_string c str ] \]
       Parse the given string using the Z3 native parser.
       
       Return the conjunction of asserts made in the input.
*)
val parse_z3_string : context -> string -> ast
	

(**
       Summary: Similar to {!parse_z3_string}, but reads the benchmark from a file.
*)
val parse_z3_file : context -> string -> ast
	

(**
       {2 {L Miscellaneous}}
*)
(**
       Summary: Return Z3 version number information.
*)
val get_version : unit -> int * int * int * int
	

(**
       Summary: Reset all allocated resources. 

       Use this facility on out-of memory errors. 
       It allows discharging the previous state and resuming afresh.
       Any pointers previously returned by the API
       become invalid.
*)
val reset_memory : unit -> unit
	

(**
        {2 {L val Theory Plugins}}
*)
(**
       Summary: Create an interpreted theory sort.
*)
val theory_mk_sort : context -> theory -> symbol -> sort
	

(**
       Summary: Create an interpreted theory constant value. Values are assumed to be different from each other.
*)
val theory_mk_value : context -> theory -> symbol -> sort -> ast
	

(**
       Summary: Create an interpreted constant for the given theory.
*)
val theory_mk_constant : context -> theory -> symbol -> sort -> ast
	

(**
       Summary: Create an interpreted function declaration for the given theory.
*)
val theory_mk_func_decl : context -> theory -> symbol -> sort array -> sort -> func_decl
	

(**
       Summary: Return the context where the given theory is installed.
*)
val theory_get_context : theory -> context
	

(**
       Summary: Assert a theory axiom/lemmas during the search.
       
       An axiom added at search level [n] will remain in the logical context until 
       level [n] is backtracked. 

       The callbacks for push ({!set_push_callback}) and pop
       ({!set_pop_callback}) can be used to track when the search
       level is increased (i.e., new case-split) and decreased (i.e.,
       case-split is backtracked).
       
       Z3 tracks the theory axioms asserted. So, multiple assertions of the same axiom are
       ignored.
*)
val theory_assert_axiom : theory -> ast -> unit
	

(**
       Summary: Inform to the logical context that [lhs] and [rhs] have the same interpretation
       in the model being built by theory [t]. If lhs = rhs is inconsistent with other theories,
       then the logical context will backtrack.

       For more information, see the paper "Model-Based Theory Combination" in the Z3 website.
*)
val theory_assume_eq : theory -> ast -> ast -> unit
	

(**
       Summary: Enable/disable the simplification of theory axioms asserted using {!theory_assert_axiom}.
       By default, the simplification of theory specific operators is disabled. 
       That is, the reduce theory callbacks are not invoked for theory axioms.
       The default behavior is useful when asserting axioms stating properties of theory operators.
*)
val theory_enable_axiom_simplification : theory -> bool -> unit
	

(**
       Summary: Return the root of the equivalence class containing [n].
*)
val theory_get_eqc_root : theory -> ast -> ast
	

(**
       Summary: Return the next element in the equivalence class containing [n].

       The elements in an equivalence class are organized in a circular list.
       You can traverse the list by calling this function multiple times 
       using the result from the previous call. This is illustrated in the
       code snippet below.
       {v 
           ast curr = n;
           do
             curr = theory_get_eqc_next(theory, curr);
           while (curr != n);
        v}
*)
val theory_get_eqc_next : theory -> ast -> ast
	

(**
       Summary: Return the number of parents of [n] that are operators of the given theory. 
*)
val theory_get_num_parents : theory -> ast -> int
	

(**
       Summary: Return the i-th parent of [n]. 
       See {!theory_get_num_parents}. 
*)
val theory_get_parent : theory -> ast -> int -> ast
	

(**
       Summary: Return [TRUE] if [n] is an interpreted theory value.
*)
val theory_is_value : theory -> ast -> bool
	

(**
       Summary: Return [TRUE] if [d] is an interpreted theory declaration.
*)
val theory_is_decl : theory -> func_decl -> bool
	

(**
       Summary: Return the number of expressions of the given theory in
       the logical context. These are the expressions notified using the
       callback {!set_new_elem_callback}.
*)
val theory_get_num_elems : theory -> int
	

(**
       Summary: Return the i-th elem of the given theory in the logical context.
       
       - {b See}: {!theory_get_num_elems}
*)
val theory_get_elem : theory -> int -> ast
	

(**
       Summary: Return the number of theory applications in the logical
       context. These are the expressions notified using the callback
       {!set_new_app_callback}.
*)
val theory_get_num_apps : theory -> int
	

(**
       Summary: Return the i-th application of the given theory in the logical context.
       
       - {b See}: {!theory_get_num_apps}
*)
val theory_get_app : theory -> int -> ast
	

(**
       {2 {L Deprecated Injective functions API}}
*)
(**
       Summary: Create injective function declaration

       @deprecated This method just asserts a (universally quantified) formula that asserts that
       the new function is injective. It is compatible with the old interface for solving:
       {!assert_cnstr}, {!check_assumptions}, etc.
*)
val mk_injective_function : context -> symbol -> sort array -> sort -> func_decl
	

(**
       {2 {L Deprecated Constraints API}}
*)
(**
       Summary: Set the SMTLIB logic to be used in the given logical context.
       It is incorrect to invoke this function after invoking
       {!check}, {!check_and_get_model}, {!check_assumptions} and {!push}.
       Return [TRUE] if the logic was changed successfully, and [FALSE] otherwise.

       @deprecated Subsumed by {!mk_solver_for_logic}
*)
val set_logic : context -> string -> bool
	

(**
        Summary: Create a backtracking point.
        
        The logical context can be viewed as a stack of contexts.  The
        scope level is the number of elements on this stack. The stack
        of contexts is simulated using trail (undo) stacks.

        - {b See also}: {!pop}

        @deprecated Subsumed by {!solver_push}
*)
val push : context -> unit
	

(**
       Summary: Backtrack.
       
       Restores the context from the top of the stack, and pops it off the
       stack.  Any changes to the logical context (by {!assert_cnstr} or
       other functions) between the matching {!push} and [pop]
       operators are flushed, and the context is completely restored to
       what it was right before the {!push}.
       
       - {b See also}: {!push}

       @deprecated Subsumed by {!solver_pop}
*)
val pop : context -> int -> unit
	

(**
       Summary: Retrieve the current scope level.
       
       It retrieves the number of scopes that have been pushed, but not yet popped.
       
       - {b See also}: {!push}
       - {b See also}: {!pop}
    
       @deprecated Subsumed by {!solver_get_num_scopes}.
*)
val get_num_scopes : context -> int
	

(**
       
       
       
       
       
       
       
       
       
       
       
       
    
       
       

        @deprecated This function has no effect. 
*)
val persist_ast : context -> ast -> int -> unit
	

(**
       Summary: Assert a constraint into the logical context.
       
       After one assertion, the logical context may become
       inconsistent.  
       
       The functions {!check} or {!check_and_get_model} should be
       used to check whether the logical context is consistent or not.

       - {b See also}: {!check}
       - {b See also}: {!check_and_get_model}

       @deprecated Subsumed by {!solver_assert}
*)
val assert_cnstr : context -> ast -> unit
	

(**
       Summary: Check whether the given logical context is consistent or not.

       If the logical context is not unsatisfiable (i.e., the return value is different from [L_FALSE)]
       and model construction is enabled (see {!mk_config}),
       
        then a valid model is returned.  Otherwise, it is unsafe to use the returned model.
       
       
       
       
       
       
       - {b Remarks}: Model construction must be enabled using configuration
       parameters (See, {!mk_config}).

       - {b See also}: {!check}
       

       @deprecated Subsumed by {!solver_check}
*)
val check_and_get_model : context -> lbool * model
	

(**
       Summary: Check whether the given logical context is consistent or not.

       The function {!check_and_get_model} should be used when models are needed.

       - {b See also}: {!check_and_get_model}
    
       @deprecated Subsumed by {!solver_check}
*)
val check : context -> lbool
	

(**
       Summary: Check whether the given logical context and optional assumptions is consistent or not.

       If the logical context is not unsatisfiable (i.e., the return value is different from [L_FALSE)],
       
       and model construction is enabled (see {!mk_config}),
       
        then a valid model is returned.  Otherwise, it is unsafe to use the returned model.
       
       
       
       

       @param c logical context.
       @param num_assumptions number of auxiliary assumptions.
       @param assumptions array of auxiliary assumptions
       @param m optional pointer to a model.
       @param proof optional pointer to a proof term.
       @param core_size size of unsatisfiable core. 
       @param core pointer to an array receiving unsatisfiable core. 
              The unsatisfiable core is a subset of the assumptions, so the array has the same size as the assumptions.
              The [core] array is not populated if [core_size] is set to 0.

       - {b Precondition}: assumptions comprises of propositional literals.
            In other words, you cannot use compound formulas for assumptions, 
            but should use propositional variables or negations of propositional variables.
              
       
       
       

       - {b See also}: {!check}
       
    
       @deprecated Subsumed by {!solver_check_assumptions}
*)
val check_assumptions : context -> ast array -> int -> ast array -> lbool * model * ast * int * ast array
	

(**
       Summary: Retrieve congruence class representatives for terms.

       The function can be used for relying on Z3 to identify equal terms under the current
       set of assumptions. The array of terms and array of class identifiers should have
       the same length. The class identifiers are numerals that are assigned to the same
       value for their corresponding terms if the current context forces the terms to be
       equal. You cannot deduce that terms corresponding to different numerals must be all different, 
       (especially when using non-convex theories).
       All implied equalities are returned by this call.
       This means that two terms map to the same class identifier if and only if
       the current context implies that they are equal.

       A side-effect of the function is a satisfiability check.
       The function return L_FALSE if the current assertions are not satisfiable.

       - {b See also}: {!check_and_get_model}
       - {b See also}: {!check}
    
       @deprecated Subsumed by solver API
*)
val get_implied_equalities : context -> ast array -> lbool * int array
	

(**
       Summary: Delete a model object.
       
       - {b See also}: {!check_and_get_model}
    
       @deprecated Subsumed by solver API
*)
val del_model : context -> model -> unit
	

(**
       {2 {L Deprecated Search control API}}
*)
(**
       Summary: Cancel an ongoing check.
       
       Notifies the current check to abort and return.
       This method should be called from a different thread
       than the one performing the check.
       
       @deprecated Use {!interrupt} instead.
*)
val soft_check_cancel : context -> unit
	

(**
       Summary: Retrieve reason for search failure.
       
       If a call to {!check} or {!check_and_get_model} returns L_UNDEF, 
       use this facility to determine the more detailed cause of search failure.

       @deprecated Subsumed by {!solver_get_reason_unknown}
*)
val get_search_failure : context -> search_failure
	

(**
       {2 {L Deprecated Labels API}}
*)
(**
       Summary: Create a labeled formula.

       @param c logical context.
       @param s name of the label.
       @param is_pos label polarity.
       @param f formula being labeled.

       A label behaves as an identity function, so the truth value of the 
       labeled formula is unchanged. Labels are used for identifying 
       useful sub-formulas when generating counter-examples.

       @deprecated Labels are only supported by the old Solver API.
       This feature is not essential (it can be simulated using auxiliary Boolean variables).
       It is only available for backward compatibility.
*)
val mk_label : context -> symbol -> bool -> ast -> ast
	

(**
        Summary: Retrieve the set of labels that were relevant in
        the context of the current satisfied context.

        - {b See also}: {!del_literals}
        - {b See also}: {!get_num_literals}
        - {b See also}: {!get_label_symbol}
        - {b See also}: {!get_literal}

        @deprecated This procedure is based on the old Solver API.
*)
val get_relevant_labels : context -> literals
	

(**
        Summary: Retrieve the set of literals that satisfy the current context.

        - {b See also}: {!del_literals}
        - {b See also}: {!get_num_literals}
        - {b See also}: {!get_label_symbol}
        - {b See also}: {!get_literal}

        @deprecated This procedure is based on the old Solver API.
*)
val get_relevant_literals : context -> literals
	

(**
        Summary: Retrieve the set of literals that whose assignment were 
        guess, but not propagated during the search.

        - {b See also}: {!del_literals}
        - {b See also}: {!get_num_literals}
        - {b See also}: {!get_label_symbol}
        - {b See also}: {!get_literal}

        @deprecated This procedure is based on the old Solver API.
*)
val get_guessed_literals : context -> literals
	

(**
       Summary: Delete a labels context.
       
       - {b See also}: {!get_relevant_labels}

        @deprecated This procedure is based on the old Solver API.
*)
val del_literals : context -> literals -> unit
	

(**
       Summary: Retrieve the number of label symbols that were returned.
       
       - {b See also}: {!get_relevant_labels}

        @deprecated This procedure is based on the old Solver API.
*)
val get_num_literals : context -> literals -> int
	

(**
       Summary: Retrieve label symbol at idx.

       @deprecated This procedure is based on the old Solver API.
*)
val get_label_symbol : context -> literals -> int -> symbol
	

(**
       Summary: Retrieve literal expression at idx.

       @deprecated This procedure is based on the old Solver API.
*)
val get_literal : context -> literals -> int -> ast
	

(**
       Summary: Disable label.
       
       The disabled label is not going to be used when blocking the subsequent search.

       - {b See also}: {!block_literals}

       @deprecated This procedure is based on the old Solver API.
*)
val disable_literal : context -> literals -> int -> unit
	

(**
       Summary: Block subsequent checks using the remaining enabled labels.

       @deprecated This procedure is based on the old Solver API.
*)
val block_literals : context -> literals -> unit
	

(**
       {2 {L Deprecated Model API}}
*)
(**
       Summary: Return the number of constants assigned by the given model.
       
        - {b Remarks}: Consider using {!get_model_constants}. 

       - {b See also}: {!get_model_constant}

       @deprecated use {!model_get_num_consts}
*)
val get_model_num_constants : context -> model -> int
	

(**
       Summary: \[ [ get_model_constant c m i ] \]
       Return the i-th constant in the given model. 

        - {b Remarks}: Consider using {!get_model_constants}. 

       - {b Precondition}: i < get_model_num_constants c m

       @deprecated use {!model_get_const_decl}
*)
val get_model_constant : context -> model -> int -> func_decl
	

(**
       Summary: Return the number of function interpretations in the given model.
       
       A function interpretation is represented as a finite map and an 'else' value.
       Each entry in the finite map represents the value of a function given a set of arguments.

       @deprecated use {!model_get_num_funcs}
*)
val get_model_num_funcs : context -> model -> int
	

(**
       Summary: \[ [ get_model_func_decl c m i ] \]
       Return the declaration of the i-th function in the given model.

       - {b Precondition}: i < get_model_num_funcs c m

       - {b See also}: {!get_model_num_funcs}

       @deprecated use {!model_get_func_decl}
*)
val get_model_func_decl : context -> model -> int -> func_decl
	

(**
       Summary: Return the value of the given constant or function 
       in the given model.
       
       @deprecated Consider using {!model_eval} or {!model_get_func_interp}
*)
val eval_func_decl : context -> model -> func_decl -> bool * ast
	

(**
       Summary: \[ [ is_array_value c v ] \]
       Determine whether the term encodes an array value.       
       A term encodes an array value if it is a nested sequence of 
       applications of store on top of a constant array.
       The indices to the stores have to be values (for example, integer constants)
       so that equality between the indices can be evaluated.
       Array values are useful for representing interpretations for arrays.
              
       Return the number of entries mapping to non-default values of the array.

       @deprecated Use {!is_as_array}
*)
val is_array_value : context -> model -> ast -> bool * int
	

(**
       Summary: \[ [ get_array_value c v ] \]
       An array values is represented as a dictionary plus a
       default (else) value. This function returns the array graph.

       - {b Precondition}: TRUE == is_array_value c v &num_entries       

       @deprecated Use func_interp objects and {!get_as_array_func_decl}
*)
val get_array_value : context -> model -> ast -> ast array -> ast array -> ast array * ast array * ast
	

(**
       Summary: \[ [ get_model_func_else c m i ] \]
       Return the 'else' value of the i-th function interpretation in the given model.
 
       A function interpretation is represented as a finite map and an 'else' value.

        - {b Remarks}: Consider using {!get_model_funcs}. 
       
       - {b Precondition}: i < get_model_num_funcs c m

       - {b See also}: {!get_model_num_funcs}
       - {b See also}: {!get_model_func_num_entries}
       - {b See also}: {!get_model_func_entry_num_args}
       - {b See also}: {!get_model_func_entry_arg}

       @deprecated Use func_interp objects
*)
val get_model_func_else : context -> model -> int -> ast
	

(**
       Summary: \[ [ get_model_func_num_entries c m i ] \]
       Return the number of entries of the i-th function interpretation in the given model.
 
       A function interpretation is represented as a finite map and an 'else' value.

        - {b Remarks}: Consider using {!get_model_funcs}. 
       
       - {b Precondition}: i < get_model_num_funcs c m

       - {b See also}: {!get_model_num_funcs}
       - {b See also}: {!get_model_func_else}
       - {b See also}: {!get_model_func_entry_num_args}
       - {b See also}: {!get_model_func_entry_arg}

       @deprecated Use func_interp objects
*)
val get_model_func_num_entries : context -> model -> int -> int
	

(**
       Summary: \[ [ get_model_func_entry_num_args c m i j ] \]
       Return the number of arguments of the j-th entry of the i-th function interpretation in the given
       model.

       A function interpretation is represented as a finite map and an 'else' value.
       This function returns the j-th entry of this map.
      
       An entry represents the value of a function given a set of arguments.
       

        - {b Remarks}: Consider using {!get_model_funcs}. 

       - {b Precondition}: i < get_model_num_funcs c m
       - {b Precondition}: j < get_model_func_num_entries c m i

       - {b See also}: {!get_model_num_funcs}
       - {b See also}: {!get_model_func_num_entries }
       - {b See also}: {!get_model_func_entry_arg}

       @deprecated Use func_interp objects
*)
val get_model_func_entry_num_args : context -> model -> int -> int -> int
	

(**
       Summary: \[ [ get_model_func_entry_arg c m i j k ] \]
       Return the k-th argument of the j-th entry of the i-th function interpretation in the given
       model.

       A function interpretation is represented as a finite map and an 'else' value.
       This function returns the j-th entry of this map.
      
       An entry represents the value of a function given a set of arguments.
       

        - {b Remarks}: Consider using {!get_model_funcs}. 

       - {b Precondition}: i < get_model_num_funcs c m
       - {b Precondition}: j < get_model_func_num_entries c m i
       - {b Precondition}: k < get_model_func_entry_num_args c m i j

       - {b See also}: {!get_model_num_funcs}
       - {b See also}: {!get_model_func_num_entries }
       - {b See also}: {!get_model_func_entry_num_args}

       @deprecated Use func_interp objects
*)
val get_model_func_entry_arg : context -> model -> int -> int -> int -> ast
	

(**
       Summary: \[ [ get_model_func_entry_value c m i j ] \]
       Return the return value of the j-th entry of the i-th function interpretation in the given
       model.

       A function interpretation is represented as a finite map and an 'else' value.
       This function returns the j-th entry of this map.
      
       An entry represents the value of a function given a set of arguments.
       

        - {b Remarks}: Consider using {!get_model_funcs}. 

       - {b Precondition}: i < get_model_num_funcs c m
       - {b Precondition}: j < get_model_func_num_entries c m i

       - {b See also}: {!get_model_num_funcs}
       - {b See also}: {!get_model_func_num_entries }

       @deprecated Use func_interp objects
*)
val get_model_func_entry_value : context -> model -> int -> int -> ast
	

(**
       Summary: \[ [ eval c m t ] \]
       Evaluate the AST node [t] in the given model. 
       
        Return a pair: Boolean and value. The Boolean is true if the term was successfully evaluated. 

       The evaluation may fail for the following reasons:

       - [t] contains a quantifier.

       - the model [m] is partial, that is, it doesn't have a complete interpretation for uninterpreted functions. 
         That is, the option {e MODEL_PARTIAL=true } was used.

       - [t] is type incorrect.

       @deprecated Use {!model_eval}
*)
val eval : context -> model -> ast -> bool * ast
	

(**
       Summary: Evaluate declaration given values.

       Provides direct way to evaluate declarations
       without going over terms.
    
       @deprecated Consider using {!model_eval} and {!substitute_vars}
*)
val eval_decl : context -> model -> func_decl -> ast array -> bool * ast
	

(**
       {2 {L Deprecated String conversion API}}
*)
(**
       Summary: Convert the given logical context into a string.
       
       This function is mainly used for debugging purposes. It displays
       the internal structure of a logical context.

       
       
       

       @deprecated This method is obsolete. It just displays the internal representation of 
       the global solver available for backward compatibility reasons.
*)
val context_to_string : context -> string
	

(**
       Summary: Return runtime statistics as a string.
       
       This function is mainly used for debugging purposes. It displays
       statistics of the search activity.

       
       
       

       @deprecated This method is based on the old solver API. 
       Use {!stats_to_string} when using the new solver API.
*)
val statistics_to_string : context -> string
	

(**
       Summary: Extract satisfying assignment from context as a conjunction.
       
       This function can be used for debugging purposes. It returns a conjunction
       of formulas that are assigned to true in the current context.
       This conjunction will contain not only the assertions that are set to true
       under the current assignment, but will also include additional literals
       if there has been a call to {!check} or {!check_and_get_model}.       
       
       @deprecated This method is based on the old solver API.
*)
val get_context_assignment : context -> ast
	



(** {2 {L ML Extensions}} *)

(**
  \[ [ mk_context_x configs] \] is a shorthand for the context with configurations in [configs].
*)
val mk_context_x: (string * string) array -> context;;

(**
  \[ [ get_app_args c a ] \] is the array of arguments of an application. If [t] is a constant, then the array is empty.

  - {b See also}: {!get_app_num_args}
  - {b See also}: {!get_app_arg}
*)
val get_app_args:  context -> app -> ast array

(**
  \[ [ get_app_args c d ] \] is the array of parameters of [d].

  - {b See also}: {!get_domain_size}
  - {b See also}: {!get_domain}
*)
val get_domains: context -> func_decl -> sort array

(**
  \[ [ get_array_sort c t ] \] is the domain and the range of [t].

  - {b See also}: {!get_array_sort_domain}
  - {b See also}: {!get_array_sort_range}
*)
val get_array_sort: context -> sort -> sort * sort

(**
  \[ [ get_tuple_sort c ty ] \] is the pair [(mk_decl, fields)] where [mk_decl] is the constructor declaration of [ty], and [fields] is the array of fields in [ty].

  - {b See also}: {!get_tuple_sort_mk_decl}
  - {b See also}: {!get_tuple_sort_num_fields}
  - {b See also}: {!get_tuple_sort_field_decl}
*)
val get_tuple_sort: context -> sort -> (func_decl * func_decl array)

(**
  \[ [ datatype_constructor_refined ] \] is the refinement of a datatype constructor.
  
  It contains the constructor declaration, recognizer, and list of accessor functions.
*)
type datatype_constructor_refined = { 
   constructor : func_decl; 
   recognizer : func_decl; 
   accessors : func_decl array 
}

(**
  \[ [ get_datatype_sort c ty ] \] is the array of triples [(constructor, recognizer, fields)] where [constructor] is the constructor declaration of [ty], [recognizer] is the recognizer for the [constructor], and [fields] is the array of fields in [ty].

  - {b See also}: {!get_datatype_sort_num_constructors}
  - {b See also}: {!get_datatype_sort_constructor}
  - {b See also}: {!get_datatype_sort_recognizer}
  - {b See also}: {!get_datatype_sort_constructor_accessor}
*)


val get_datatype_sort: context -> sort -> datatype_constructor_refined array

(**
  \[ [ get_model_constants c m ] \] is the array of constants in the model [m].

  - {b See also}: {!get_model_num_constants}
  - {b See also}: {!get_model_constant}
*)
val get_model_constants: context -> model -> func_decl array


(**
  \[ [ get_model_func_entry c m i j ] \] is the [j]'th entry in the [i]'th function in the model [m].

  - {b See also}: {!get_model_func_entry_num_args}
  - {b See also}: {!get_model_func_entry_arg}
  - {b See also}: {!get_model_func_entry_value}
*)
val get_model_func_entry: context -> model -> int -> int -> (ast array * ast);;

(**
  \[ [ get_model_func_entries c m i ] \] is the array of entries in the [i]'th function in the model [m].

  - {b See also}: {!get_model_func_num_entries}
  - {b See also}: {!get_model_func_entry}
*)
val get_model_func_entries: context -> model -> int -> (ast array * ast) array;;

(**
  \[ [ get_model_funcs c m ] \] is the array of functions in the model [m]. Each function is represented by the triple [(decl, entries, else)], where [decl] is the declaration name for the function, [entries] is the array of entries in the function, and [else] is the default (else) value for the function.

  - {b See also}: {!get_model_num_funcs}
  - {b See also}: {!get_model_func_decl}
  - {b See also}: {!get_model_func_entries}
  - {b See also}: {!get_model_func_else}
*)
val get_model_funcs: context -> model -> 
  (symbol *
   (ast array * ast) array * 
   ast) array

(**
  \[ [ get_smtlib_formulas c ] \] is the array of formulas created by a preceding call to {!parse_smtlib_string} or {!parse_smtlib_file}.

  Recommend use {!parse_smtlib_string_x} or {!parse_smtlib_file_x} for functional style interface to the SMT-LIB parser.

  - {b See also}: {!parse_smtlib_string_x}
  - {b See also}: {!parse_smtlib_file_x}
  - {b See also}: {!parse_smtlib_string}
  - {b See also}: {!parse_smtlib_file}
  - {b See also}: {!get_smtlib_num_formulas}
  - {b See also}: {!get_smtlib_formula}
*)
val get_smtlib_formulas: context -> ast array

(**
  \[ [get_smtlib_assumptions c] \] is the array of assumptions created by a preceding call to {!parse_smtlib_string} or {!parse_smtlib_file}.

  Recommend use {!parse_smtlib_string_x} or {!parse_smtlib_file_x} for functional style interface to the SMT-LIB parser.


  - {b See also}: {!parse_smtlib_string_x}
  - {b See also}: {!parse_smtlib_file_x}
  - {b See also}: {!parse_smtlib_string}
  - {b See also}: {!parse_smtlib_file}
  - {b See also}: {!get_smtlib_num_assumptions}
  - {b See also}: {!get_smtlib_assumption}
*)
val get_smtlib_assumptions: context -> ast array

(**
  \[ [ get_smtlib_decls c ] \] is the array of declarations created by a preceding call to {!parse_smtlib_string} or {!parse_smtlib_file}.

  Recommend use {!parse_smtlib_string_x} or {!parse_smtlib_file_x} for functional style interface to the SMT-LIB parser.


  - {b See also}: {!parse_smtlib_string_x}
  - {b See also}: {!parse_smtlib_file_x}
  - {b See also}: {!parse_smtlib_string}
  - {b See also}: {!parse_smtlib_file}
  - {b See also}: {!get_smtlib_num_decls}
  - {b See also}: {!get_smtlib_decl}
*)
val get_smtlib_decls: context -> func_decl array

(**
  \[ [ get_smtlib_parse_results c ] \] is the triple [(get_smtlib_formulas c, get_smtlib_assumptions c, get_smtlib_decls c)].

  Recommend use {!parse_smtlib_string_x} or {!parse_smtlib_file_x} for functional style interface to the SMT-LIB parser.


  - {b See also}: {!parse_smtlib_string_x}
  - {b See also}: {!parse_smtlib_file_x}
  - {b See also}: {!parse_smtlib_string}
  - {b See also}: {!parse_smtlib_file}
  - {b See also}: {!get_smtlib_formulas}
  - {b See also}: {!get_smtlib_assumptions}
  - {b See also}: {!get_smtlib_decls}
*)
val get_smtlib_parse_results: context -> (ast array * ast array * func_decl array)

(**
  \[ [ parse_smtlib_string_formula c ... ] \] calls [(parse_smtlib_string c ...)] and returns the single formula produced. 

  Recommended for functional style interface to the SMT-LIB parser.

  - {b See also}: {!parse_smtlib_file_formula}
  - {b See also}: {!parse_smtlib_string_x}
*)
val parse_smtlib_string_formula: context -> string -> symbol array -> sort array -> symbol array -> func_decl array -> ast

(**
  \[ [ parse_smtlib_file_formula c ... ] \] calls [(parse_smtlib_file c ...)] and returns the single formula produced. 

  Recommended for functional style interface to the SMT-LIB parser.

  - {b See also}: {!parse_smtlib_file_formula}
  - {b See also}: {!parse_smtlib_file_x}
*)
val parse_smtlib_file_formula: context -> string -> symbol array -> sort array -> symbol array -> func_decl array -> ast

(**
  \[ [ parse_smtlib_string_x c ... ] \] is [(parse_smtlib_string c ...; get_smtlib_parse_results c)]

  Recommended for functional style interface to the SMT-LIB parser.

  - {b See also}: {!parse_smtlib_file_x}
  - {b See also}: {!parse_smtlib_string}
  - {b See also}: {!get_smtlib_parse_results}
*)
val parse_smtlib_string_x: context -> string -> symbol array -> sort array -> symbol array -> func_decl array -> (ast array * ast array * func_decl array)

(**
  \[ [ parse_smtlib_file_x c ... ] \] is [(parse_smtlib_file c ...; get_smtlib_parse_results c)]

  Recommended for functional style interface to the SMT-LIB parser.

  - {b See also}: {!parse_smtlib_string_x}
  - {b See also}: {!parse_smtlib_file}
  - {b See also}: {!get_smtlib_parse_results}
*)
val parse_smtlib_file_x: context -> string -> symbol array -> sort array -> symbol array -> func_decl array -> (ast array * ast array * func_decl array)

(**
  \[ [ symbol_refined ] \] is the refinement of a {!symbol} .

  - {b See also}: {!symbol_refine}
  - {b See also}: {!get_symbol_kind}
*)
type symbol_refined =
  | Symbol_int of int
  | Symbol_string of string
  | Symbol_unknown;;

(**
  \[ [ symbol_refine c s ] \] is the refined symbol of [s].

  - {b See also}:  {!symbol_refined}
  - {b See also}: {!get_symbol_kind}
*)
val symbol_refine: context -> symbol -> symbol_refined;;

(**
  \[ [ sort_refined ] \] is the refinement of a {!sort} .

  - {b See also}: {!sort_refine}
  - {b See also}: {!get_sort_kind}
*)


type sort_refined =
  | Sort_uninterpreted of symbol
  | Sort_bool
  | Sort_int
  | Sort_real
  | Sort_bv of int
  | Sort_array of (sort * sort)
  | Sort_datatype of datatype_constructor_refined array
  | Sort_relation
  | Sort_finite_domain
  | Sort_unknown of symbol

(**
  \[ [ sort_refine c t ] \] is the refined sort of [t].

  - {b See also}:  {!sort_refined}
  - {b See also}: {!get_sort_kind}
*)
val sort_refine: context -> sort -> sort_refined;;

(**
  \[ [ binder_type ] \] is a universal or existential quantifier.

  - {b See also}: {!term_refined}
*)
type binder_type = | Forall | Exists 

(**
  \[ [ numeral_refined ] \] is the refinement of a numeral .

  Numerals whose fractional representation can be fit with
  64 bit integers are treated as small.

*)
type numeral_refined = 
  | Numeral_small  of int64 * int64
  | Numeral_large  of string

(**
  \[ [ term_refined ] \] is the refinement of a {!ast} .

  - {b See also}: {!term_refine}
*)
type term_refined = 
  | Term_app        of decl_kind * func_decl * ast array
  | Term_quantifier of binder_type * int * ast array array * (symbol * sort) array * ast
  | Term_numeral    of numeral_refined * sort
  | Term_var        of int * sort

(**
  \[ [ term_refine c a ] \] is the refined term of [a].

  - {b See also}:  {!term_refined}
*)
val term_refine : context -> ast -> term_refined

(** 
  \[ [mk_theory c name ] \] create a custom theory.

*)
val mk_theory : context -> string -> theory

(**
  \[ [set_delete_callback th cb] \] set callback when theory gets deleted.
*)
val set_delete_callback : theory -> (unit -> unit) -> unit

(**
  \[ [set_reduce_app_callback th cb] \] set callback for simplifying theory terms.
*)
val set_reduce_app_callback : theory -> (func_decl -> ast array -> ast option) -> unit

(**
  \[ [set_reduce_eq_callback th cb] \] set callback for simplifying equalities over theory terms.
*)
val set_reduce_eq_callback : theory -> (ast -> ast -> ast option) -> unit

(**
  \[ [set_reduce_distinct_callback th cb] \] set callback for simplifying disequalities over theory terms.
*)
val set_reduce_distinct_callback : theory -> (ast array -> ast option) -> unit

(**
  \[ [set_new_app_callback th cb] \] set callback for registering new application.
*)
val set_new_app_callback : theory -> (ast -> unit) -> unit

(**
  \[ [set_new_elem_callback th cb] \] set callback for registering new element.

  - {b See also}: the help for the corresponding C API function.  
*)
val set_new_elem_callback : theory -> (ast -> unit) -> unit

(**
  \[ [set_init_search_callback th cb] \] set callback when Z3 starts searching for a satisfying assignment.
*)
val set_init_search_callback : theory -> (unit -> unit) -> unit

(**
  \[ [set_push_callback th cb] \] set callback for a logical context push.
*)
val set_push_callback : theory -> (unit -> unit) -> unit

(**
  \[ [set_pop_callback th cb] \] set callback for a logical context pop.
*)
val set_pop_callback : theory -> (unit -> unit) -> unit

(**
  \[ [set_restart_callback th cb] \] set callback for search restart.
*)
val set_restart_callback : theory -> (unit -> unit) -> unit

val set_reset_callback : theory -> (unit -> unit) -> unit

val set_final_check_callback : theory -> (unit -> bool) -> unit

val set_new_eq_callback : theory -> (ast -> ast -> unit) -> unit

val set_new_diseq_callback : theory -> (ast -> ast -> unit) -> unit

val set_new_assignment_callback : theory -> (ast -> bool -> unit) -> unit

val set_new_relevant_callback : theory -> (ast -> unit) -> unit



end

