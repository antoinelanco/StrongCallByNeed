open Printer

type var = string

and terms =
  | Lambda of var * terms (* λx.t *)
  | App    of terms * terms (* t1 t2 *)
  | Var    of var (* x *)
  | ES     of terms * var * terms (* t[x/u] *)


and context =
  | C_Lambda of var (* λx.◽ *)
  | C_AppG   of terms (* ◽ t2 *)
  | C_AppD   of terms (* t1 ◽ *)
  | C_ESG    of var * terms (* ◽[x/t] *)
  | C_ESD    of terms * var (* t[x/◽] *)
  | C_HOLE (* ◽ *)

and pair = terms * context list

let rec sprint_terms = function
  | Lambda(x,t) -> Printf.sprintf "λ%s.%s" x (sprint_terms t)
  | App(t1,t2) -> Printf.sprintf "(%s) (%s)" (sprint_terms t1) (sprint_terms t2)
  | Var(x) -> x
  | ES(t1,x,t2) -> Printf.sprintf "(%s)[%s/%s]" (sprint_terms t1) x (sprint_terms t2)

let rec sprint_context = function
  | C_Lambda v -> Printf.sprintf "λ%s.◽" v
  | C_AppG t -> Printf.sprintf "◽ (%s)" (sprint_terms t)
  | C_AppD t -> Printf.sprintf "(%s) ◽" (sprint_terms t)
  | C_ESG (v,t) -> Printf.sprintf "◽ [%s/%s]" v (sprint_terms t)
  | C_ESD (t,v) -> Printf.sprintf "(%s)[%s/◽]" (sprint_terms t) v
  | C_HOLE -> "◽"

let sprint_all (t,c) =
  Printf.sprintf "%s %s" (sprint_terms t) (print_tab c sprint_context)
