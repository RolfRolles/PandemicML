%{
  open Z3TextualParsedModel

  type unresolved_t =
  | URBitVector of bvpair
  | URArray of arrayt list
  | URAsArray of string 
%}

%token<string> ID
%token<string * string> BV
%token EOF LSQUARE RSQUARE LBRACE RBRACE ASARRAY ARROW

%type <(string * Z3TextualParsedModel.t) list> model

%start model

%%
revarraylist:
| revarraylist BV ARROW BV { (RealEntry($2,$4))::$1 }
| revarraylist ID ARROW BV 
{ 
  match $2 with 
  | "else" -> (DefaultEntry($4))::$1 
  | _ -> failwith ("parsing z3 model:  invalid array member "^$2) 
}
| { [] }

model: 
| modelelementlist EOF 
{ 
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
  resolve_array_refs $1 
}

modelelementlist:
| modelelementlist modelelement { $2 :: $1 }
| { [] }

modelelement:
| ID ARROW BV       { ($1,URBitVector($3)) }
| ID ARROW ASARRAY LSQUARE ID RSQUARE { ($1,URAsArray($5)) }
| ID ARROW LBRACE revarraylist RBRACE { ($1,URArray($4)) }

%%