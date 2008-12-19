let print = false;;

let filename = 
  if Array.length Sys.argv != 2
  then 
    (print_string "Usage: ./pcgsl sourcefile\n";
    exit 1)
  else Sys.argv.(1);;
let chan = open_in filename;;
let buf = String.create (in_channel_length chan);;
let a = really_input chan buf 0 (String.length buf);;
let lexbuf = Lexing.from_string buf;;
let (sdecl, _) = Parser.program Scanner.token lexbuf;;
let fullprogram = List.fold_left
    (fun buildup s ->
      let chan1 = open_in s in
      let buf1 = String.create (in_channel_length chan1) in
      let _ = really_input chan1 buf1 0 (String.length buf1) in
      buildup ^ buf1)
    buf
    sdecl.Ast.incl.Ast.includes;;
let lexbuf = Lexing.from_string fullprogram;;
let program = Parser.program Scanner.token lexbuf;;
let _ =  
  if print then 
    let listing = Printer.string_of_program program in
    print_string listing 
  else
    ignore (Interpret.run program)
      
