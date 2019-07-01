%{
%}

%token <string> FILENAME
%token COLON
%token EOF
%token EOL

%start main             /* the entry point */
%type <(string * string list) list> main
%type <string list> sequence
%type <(string * string list) list> lines
%%
main:
  | lines EOF { $1 }
;

lines:
  | FILENAME COLON sequence EOL lines { ($1, $3) :: $5 }
  | FILENAME COLON sequence EOL { [($1, $3)] }
  | FILENAME COLON sequence     { [($1, $3)] }
  | FILENAME COLON EOL          { [($1, [])] }
  | FILENAME COLON EOL lines    { ($1, []) :: $4 }
  | FILENAME COLON              { [($1, [])] }
;

sequence:
  | FILENAME sequence { $1 :: $2 }
  | FILENAME          { [ $1 ] }
;
