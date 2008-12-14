{ open Parser }

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf }
| "//"     { comment lexbuf }
| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| '['      { LBRACK }
| ']'      { RBRACK }
| ';'      { SEMI }
| ','      { COMMA }
| "<-"     { TRANSFER }
| "++"     { PLUSTWO }
| '+'      { PLUS }
| "--"     { MINUSTWO }
| '-'      { MINUS }
| '*'      { TIMES }
| '/'      { DIVIDE }
| "+="     { PLUSEQ }
| "-="     { MINUSEQ }
| "*="     { TIMESEQ }
| "/="     { DIVIDEEQ }
| "=="     { EQ }
| '='      { ASSIGN }
| "!="     { NEQ }
| '<'      { LT }
| "<="     { LEQ }
| ">"      { GT }
| ">="     { GEQ }
| "&&"     { AND }
| "||"     { OR }
| '~'      { TILDE }
| '^'      { CONCAT }
| '#'      { GLOBALVAR }
| '$'      { ENTITYVAR }
| "<<"     { PRINT }
| ">>"     { READ }
| "bool"             { BOOL }
| "break"            { BREAK }
| "Card"             { CARD }
| "CardEntity"       { CARDENTITY }
| "CardEntities"     { CARDENTITIES }
| "continue"         { CONTINUE }
| "else"             { ELSE }
| "false"            { FALSE(false) }
| "for"              { FOR }
| "Globals"          { GLOBALS }
| "if"               { IF }
| "Include"          { INCLUDE }
| "int"              { INT }
| "list"             { LIST }
| "null"             { NULL }
| "Play"             { PLAY }
| "return"           { RETURN }
| "Start"            { START }
| "string"           { STRING }
| "true"             { TRUE(true) }
| "while"            { WHILE }
| "WinCondition" { WINCONDITION }
| ['H' 'D' 'C' 'S']("J" | "Q" | "K" | "A" | "10" | ['2'-'9']) as
    lxm { CARDLITERAL(lxm) }
| ['0'-'9']+ as 
    lxm { INTLITERAL(int_of_string lxm) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as 
    lxm { ID(lxm) }
| '\"'[^ '\n' '\t' '\b' '\r' '\\' '\'' '\"']*'\"' as 
    lxm { STRINGLITERAL(String.sub lxm 1 ((String.length lxm) - 2)) }
| ['a'-'z' 'A'-'Z' '0'-'9' '_' '.']+ as
    lxm { FILE(lxm) }
| eof { EOF }
| _ as 
    char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  '\n' { token lexbuf }
| _    { comment lexbuf }
