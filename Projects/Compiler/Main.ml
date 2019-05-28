let string_of_type_decl f_base e = let open TypeDecl in
  let rec aux = function
  | Type(a)     -> f_base a
  | Arrow(l,r)  -> (aux l)^" -> "^(aux r)
  | Tuple(l,r)  -> "("^(aux l)^" * "^(aux r)^")"
  in 
  aux e

let string_of_variant f_variant f_base = function
| v,TypeDecl.NoRecord  -> f_variant v
| v,TypeDecl.Record(t) -> f_variant v^" of "^(string_of_type_decl f_base t)

let string_of_struct_type f_struct f_base l =
  let b = Buffer.create 1000 in
  List.iter (fun (en,et) -> 
    Buffer.add_string b (f_struct en); 
    Buffer.add_string b ": "; 
    Buffer.add_string b (string_of_type_decl f_base et); 
    Buffer.add_string b "; ")
    l;
  Buffer.contents b
  
let string_of_variant_type_list f_variant f_base l =
  let b = Buffer.create 1000 in
  List.iter (fun en -> 
    Buffer.add_string b "| "; 
    Buffer.add_string b (string_of_variant f_variant f_base en); 
    Buffer.add_char b ' ') 
    l;
  Buffer.contents b

let string_of_named_type f_basename f_base f_structname f_struct f_variantname f_variant t =
  let open TypeDecl in
  let aux = function
  | NamedType(d,t)    -> (f_basename    d)^" = "^(string_of_type_decl f_base t)
  | NamedStruct(e,t)  -> (f_structname  e)^" = { "^(string_of_struct_type f_struct f_base t)^"}"
  | NamedVariant(f,t) -> (f_variantname f)^" = "^(string_of_variant_type_list f_variant f_base t)
  in 
  "type "^(aux t)

let _ = 
  let id x = x in
  List.iter (fun t -> Printf.printf "%s\n" (string_of_named_type id id id id id id t))
   (Parser.top Lexer.token (Lexing.from_string "type blah = | Foo of blee | Bar type s = { fag: gag; hag: bloo; }"))