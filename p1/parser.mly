%{ open Dirs %}

%token LEFT RIGHT EOF
%token <int> INT

%start <Dirs.t list> dirs
%%

dirs:
  | list(dir) EOF { $1 }
  ;

dir:
  | LEFT d = INT { Left d }
  | RIGHT d = INT { Right d }
  ;
