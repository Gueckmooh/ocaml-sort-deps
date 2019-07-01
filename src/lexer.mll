(* File lexer.mll *)
{
  open Parser        (* The type token is defined in parser.mli *)
}

rule token = parse
  | [' ' '\t' ]     { token lexbuf }     (* skip blanks *)
  | ['\n']          { EOL }
  | ['.''a'-'z''A'-'Z''0'-'9''/''_''-']+ as s { FILENAME s }
  | ':'            { COLON }
  | eof            { EOF }
