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


let () = Printf.printf "%s : \n\n" (sprint_terms t)

let l = context t
let () = Context.iter (fun (x,c) ->
    Printf.printf "%s -> %s\n\n" (sprint_all (x,c))
      (print_tab (eval x) (fun z -> sprint_terms (assemble (z,c)) ))
    ) l
