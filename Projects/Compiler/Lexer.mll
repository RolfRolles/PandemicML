{
  open Parser
}

let ucname = ['A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*
let lcname = ['a'-'z']['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule token = parse
| eof           { EOF }
| '\n'          { token lexbuf }
| [' ' '\t']+   { token lexbuf }

| "type"        { TYPE }
| "of"          { OF }

| "|"           { PIPE }
| "{"           { LBRACE }
| "}"           { RBRACE }
| "("           { LPAREN }
| ")"           { RPAREN }
| ";"           { SEMICOLON }
| ":"           { COLON }
| "="           { EQ }
| "->"          { ARROW }
| "*"           { TIMES }

| ucname as c   { UCNAME(c) }
| lcname as c   { LCNAME(c) }
