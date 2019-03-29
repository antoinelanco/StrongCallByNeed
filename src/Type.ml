type var = string

and terms =
  | T_Lambda of var * terms
  | T_App    of terms * terms
  | T_Var    of var
  | T_ES     of terms * var * terms

and hole = HOLE

and context =
  | C_Lambda of var * hole
  | C_AppG   of hole * terms
  | C_AppD   of terms * hole
  | C_ESG    of hole * var * terms
  | C_ESD    of terms * var * hole
  | C_HOLE   of hole

and pair = terms * context list

let rec sprint_terms = function
  | T_Lambda(x,t) -> Printf.sprintf "λ%s.%s" x (sprint_terms t)
  | T_App(t1,t2) -> Printf.sprintf "(%s) (%s)" (sprint_terms t1) (sprint_terms t2)
  | T_Var(x) -> x
  | T_ES(t1,x,t2) -> Printf.sprintf "(%s)[%s/%s]" (sprint_terms t1) x (sprint_terms t2)
