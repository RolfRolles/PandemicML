%{
open TypeDecl
%}

%token<string> UCNAME LCNAME
%token EOF LPAREN RPAREN LBRACE RBRACE TYPE OF PIPE SEMICOLON COLON EQ ARROW TIMES

%type <(string,string,string,string,string,string) TypeDecl.named_type list> top

%start top

%%
typename:
| LCNAME                  { Type($1) }
| LPAREN typename RPAREN  { $2 }
| typename TIMES typename { Tuple($1,$3) }
| typename ARROW typename { Arrow($1,$3) }

recordel:
| LCNAME COLON typename SEMICOLON { ($1,$3) }

recordellist:
| recordel recordellist { $1 :: $2 }
| { [] }

variant:
| UCNAME                { ($1,NoRecord) } 
| UCNAME OF typename    { ($1,Record($3)) }

variantlist1:
| PIPE variant variantlist1 { $2::$3 }
| { [] }

variantlist:
| variantlist1 { $1 }
| variant variantlist1 { $1::$2 }

typedecl:
| TYPE LCNAME EQ LBRACE recordellist RBRACE { NamedStruct($2,$5) } 
| TYPE LCNAME EQ variantlist                { NamedVariant($2,$4) }

typedecllist:
| typedecl typedecllist { $1 :: $2 }
| { [] }

top: 
| typedecllist EOF { $1 }

%%