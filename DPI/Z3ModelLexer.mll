{
  open Z3ModelParser
}

let digit = ['0'-'9']
let charstr = ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_' '!' '$']*

rule token = parse
| eof           { EOF }
| '\n'          { token lexbuf }
| [' ' '\t']+   { token lexbuf }
| "bv" (digit+ as value) "[" (digit+ as size) "]" { BV(size,value) }
| "->"          { ARROW }
| "["           { LSQUARE }
| "]"           { RSQUARE }
| "{"           { LBRACE }
| "}"           { RBRACE }
| "as-array"    { ASARRAY }
| charstr as c  { ID(c) }
