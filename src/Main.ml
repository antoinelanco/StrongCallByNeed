open Type
open Printf
open Fun


let id s = T_Lambda(s,T_Var s)

let t = T_App(T_Lambda("x",id "z"),T_Var("y"))
let t = T_App(T_ES(T_Lambda("y",T_App(T_Var("y"),T_Var("x"))),"x",id "z"), id "z'")

let t = T_Lambda("y",T_App(T_Var("y"),T_App(T_Lambda("x",T_Var("x")),T_Var("z"))))
let t = T_App(T_Var("y"),T_App(T_Lambda("x",T_Var("x")),T_Var("z")))

let t = T_App(
    T_Lambda("x",
           T_App(
             T_App(T_Var "x",id "z"),
             T_App(id "z'",T_Var "w")
           )),
    T_Lambda("a",id "b")
  )

let t = T_ES(T_Lambda("y",T_App(T_Var "y",T_Var "x")),"x",T_Var "z")

let t = T_ES(T_Var "x","x",T_Lambda("y",T_App(T_Var "y", T_App(id "z",T_Var "a")     )))


let () = Printf.printf "%s : \n\n" (sprint_terms t)

let l = context t
(* let () = Context.iter (fun x ->
    Printf.printf "%s -> %s\n" (sprint_terms x) (sprint_terms (eval x))) l *)

let () = Context.iter (fun x -> Printf.printf "%s\n" (sprint_all x) ) l
