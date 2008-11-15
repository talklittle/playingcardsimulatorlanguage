{ open Parser }

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf }
| "//"     { comment lexbuf }
| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| ';'      { SEMI }
| ','      { COMMA }
| '+'      { PLUS }
| '-'      { MINUS }
| '*'      { TIMES }
| '/'      { DIVIDE }
| "+="     { PLUSEQ }
| "-="     { MINUSEQ }
| "*="     { TIMESEQ }
| "/="     { DIVIDEEQ }
| "<-"     { ASSIGN }
| "->"     { TRANSFER }
| "=="     { EQ }
| "!="     { NEQ }
| '<'      { LT }
| "<="     { LEQ }
| ">"      { GT }
| ">="     { GEQ }
| "&&"     { AND }
| "||"     { OR }
| "$"      { ENTITYVAR }
| "#"      { GLOBALVAR }
| "."      { ENTITYMEM }
| ">>"     { PRINT }
| "<<"     { READ }
| "bool"             { BOOL }
| "break"            { BREAK }
| "Card"             { CARD }
| "CardEntities"     { CARDENTITIES }
| "continue"         { CONTINUE }
| "else"             { ELSE }
| "false"            { FALSE }
| "for"              { FOR }
| "Globals"          { GLOBALS }
| "if"               { IF }
| "Include"          { INCLUDE }
| "int"              { INT }
| "list"             { LIST }
| "me"               { ME }
| "null"             { NULL }
| "PlayOrder"        { PLAYORDER }
| "return"           { RETURN }
| "Start"            { START }
| "string"           { STRING }
| "true"             { TRUE }
| "var"              { VAR }
| "while"            { WHILE }
| "WinningCondition" { WINNINGCONDITION }
| ['0'-'9']+ as 
    lxm { INTLITERAL(int_of_string lxm) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as 
    lxm { ID(lxm) }
| '\"'[^ '\n' '\t' '\b' '\r' '\\' '\'' '\"']*'\"' as 
    lxm { STRINGLITERAL(String.sub lxm 1 ((String.length lxm) - 1)) }
| eof { EOF }
| _ as 
    char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  '\n' { token lexbuf }
| _    { comment lexbuf }
