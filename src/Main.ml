open Type
open Printf
open Fun
open Printer

let id s = Lambda(s,Var s)

let t = App(Lambda("x",id "z"),Var("y"))
let t = App(ES(Lambda("y",App(Var("y"),Var("x"))),"x",id "z"), id "z'")

let t = Lambda("y",App(Var("y"),App(Lambda("x",Var("x")),Var("z"))))
let t = App(Var("y"),App(Lambda("x",Var("x")),Var("z")))

let t = App(
    Lambda("x",
           App(
             App(Var "x",id "z"),
             App(id "z'",Var "w")
           )),
    Lambda("a",id "b")
  )

let t = ES(Lambda("y",App(Var "y",Var "x")),"x",Var "z")

let t = ES(Var "x","x",Lambda("y",App(Var "y", App(id "z",Var "a")     )))

let t = Lambda("x",App(Var "x",Var "x"))

let t = Lambda("a",ES(Lambda("x",App(Var "x", Var "x")),"x",id "y"))



let t = ES(Var "x","x",ES(id "z","a",Var "e"))

let () = Printf.printf "%s : \n\n" (sprint_terms t)

let l = context false t
let () = Context.iter (fun (x,c,at) ->
    Printf.printf "%s:%b -> %s\n\n" (sprint_all (x,c)) at
      (print_tab (eval at x) (fun z -> sprint_terms (assemble (z,c)) ))
    ) l


let () = print_string "---------------------------\n"


(* let t = Lambda("x",App(Var "x" ,App(id "y", Var "x")))

let () = Printf.printf "%b\n" (is_nf true t) *)
