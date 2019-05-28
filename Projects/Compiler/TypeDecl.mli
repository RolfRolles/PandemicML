(*
'a:  Base type representation as an OCaml type
'b:  Structure field member representation as an OCaml type
'c:  Variant element representation as an OCaml type
*)
type 'a type_decl =
| Type    of  'a
| Arrow   of  'a type_decl * 'a type_decl
| Tuple   of  'a type_decl * 'a type_decl

type 'a variant =
| NoRecord
| Record of 'a type_decl (* ' *)

type ('a,'b) variant_type = ('b * ('a variant)) list
type ('a,'c) struct_type  = ('c * ('a type_decl)) list

(* As above, plus:
'd:  Type name representation as an OCaml type
'e:  Struct name representation as an OCaml type
'f:  Variant name representation as an OCaml type
*)
type ('a,'b,'c,'d,'e,'f) named_type =
| NamedType    of 'd * 'a type_decl
| NamedStruct  of 'e * ('a,'b) struct_type
| NamedVariant of 'f * ('a,'c) variant_type

