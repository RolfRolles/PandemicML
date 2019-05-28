type token =
  | ID of (string)
  | BV of (string * string)
  | EOF
  | LSQUARE
  | RSQUARE
  | LBRACE
  | RBRACE
  | ASARRAY
  | ARROW

open Parsing;;
# 2 "Z3ModelParser.mly"
  open Z3TextualParsedModel

  type unresolved_t =
  | URBitVector of bvpair
  | URArray of arrayt list
  | URAsArray of string 
# 21 "Z3ModelParser.ml"
let yytransl_const = [|
    0 (* EOF *);
  259 (* LSQUARE *);
  260 (* RSQUARE *);
  261 (* LBRACE *);
  262 (* RBRACE *);
  263 (* ASARRAY *);
  264 (* ARROW *);
    0|]

let yytransl_block = [|
  257 (* ID *);
  258 (* BV *);
    0|]

let yylhs = "\255\255\
\002\000\002\000\002\000\001\000\003\000\003\000\004\000\004\000\
\004\000\000\000"

let yylen = "\002\000\
\004\000\004\000\000\000\002\000\002\000\000\000\003\000\006\000\
\005\000\002\000"

let yydefred = "\000\000\
\006\000\000\000\010\000\000\000\000\000\004\000\005\000\000\000\
\007\000\003\000\000\000\000\000\000\000\000\000\000\000\009\000\
\000\000\000\000\000\000\008\000\002\000\001\000"

let yydgoto = "\002\000\
\003\000\012\000\004\000\007\000"

let yysindex = "\003\000\
\000\000\000\000\000\000\001\000\250\254\000\000\000\000\254\254\
\000\000\000\000\006\255\005\255\007\255\002\255\004\255\000\000\
\009\255\012\255\013\255\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\000\000"

let yytablesize = 258
let yytable = "\009\000\
\006\000\008\000\010\000\001\000\011\000\014\000\015\000\017\000\
\013\000\018\000\016\000\019\000\020\000\021\000\022\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\005\000"

let yycheck = "\002\001\
\000\000\008\001\005\001\001\000\007\001\001\001\002\001\001\001\
\003\001\008\001\006\001\008\001\004\001\002\001\002\001\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\001\001"

let yynames_const = "\
  EOF\000\
  LSQUARE\000\
  RSQUARE\000\
  LBRACE\000\
  RBRACE\000\
  ASARRAY\000\
  ARROW\000\
  "

let yynames_block = "\
  ID\000\
  BV\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'revarraylist) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string * string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string * string) in
    Obj.repr(
# 20 "Z3ModelParser.mly"
                           ( (RealEntry(_2,_4))::_1 )
# 161 "Z3ModelParser.ml"
               : 'revarraylist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'revarraylist) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string * string) in
    Obj.repr(
# 22 "Z3ModelParser.mly"
( 
  match _2 with 
  | "else" -> (DefaultEntry(_4))::_1 
  | _ -> failwith ("parsing z3 model:  invalid array member "^_2) 
)
# 174 "Z3ModelParser.ml"
               : 'revarraylist))
; (fun __caml_parser_env ->
    Obj.repr(
# 27 "Z3ModelParser.mly"
  ( [] )
# 180 "Z3ModelParser.ml"
               : 'revarraylist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'modelelementlist) in
    Obj.repr(
# 31 "Z3ModelParser.mly"
( 
  (* This function duplicated from ListUtil; copied to reduce dependencies.  Refactor later. *)
  let map_filter f_opt list =
    List.fold_left (fun acc el -> match (f_opt el) with | Some(x) -> (x::acc) | None -> acc) [] (List.rev list)
  in
  let resolve_array_refs list = 
    let arrays = map_filter (function | name,URArray(l) -> Some(name,l) | _ -> None) list in
    map_filter (function
    | (n,URBitVector(p))  -> Some(n,BitVector(p))
    | (n,URAsArray(name)) -> 
     (match (List.filter (function | (x,l) when x = name -> true | _ -> false) arrays) with
      | (_,contents)::[] -> Some(n,Array(contents))
      | l -> failwith ("resolve_array_refs:  expecting 1 array for \""^name^"\", not "^(string_of_int (List.length l))))
    | (_,URArray(_)) -> None)
    list
  in
  resolve_array_refs _1 
)
# 204 "Z3ModelParser.ml"
               : (string * Z3TextualParsedModel.t) list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'modelelementlist) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'modelelement) in
    Obj.repr(
# 51 "Z3ModelParser.mly"
                                ( _2 :: _1 )
# 212 "Z3ModelParser.ml"
               : 'modelelementlist))
; (fun __caml_parser_env ->
    Obj.repr(
# 52 "Z3ModelParser.mly"
  ( [] )
# 218 "Z3ModelParser.ml"
               : 'modelelementlist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string * string) in
    Obj.repr(
# 55 "Z3ModelParser.mly"
                    ( (_1,URBitVector(_3)) )
# 226 "Z3ModelParser.ml"
               : 'modelelement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 56 "Z3ModelParser.mly"
                                      ( (_1,URAsArray(_5)) )
# 234 "Z3ModelParser.ml"
               : 'modelelement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'revarraylist) in
    Obj.repr(
# 57 "Z3ModelParser.mly"
                                      ( (_1,URArray(_4)) )
# 242 "Z3ModelParser.ml"
               : 'modelelement))
(* Entry model *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let model (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : (string * Z3TextualParsedModel.t) list)
;;
