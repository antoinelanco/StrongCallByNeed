open Type
open Printf
open Fun
open Printer

let id s = Lambda(s,Var s) (* identité *)
let k s1 s2 = Lambda(s1,Lambda(s2,Var s1)) (* λxy.x *)


let t = App(Lambda("x",id "z"),Var("y")) (* λx.(λz.z) y *)

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

(* let t = Lambda("x",App(Var "x",Var "x")) *)

(* let t = ES(Var "x","x",Lambda("y",App(Var "y",App(id "a",Var "z"))))

let t = ES(Lambda("y",App(App(Var "y", Var "x"), Var "x")),"x",id "a") *)

(* let t = ES(Var "x","x",ES(id "z","a",Var "e")) *)

(* let t = Var "x" *)

(* let () = Printf.printf "%s : \n\n" (sprint_terms t)

let l = context false t
let () = Context.iter (fun (x,c,at) ->
    Printf.printf "%s:%b -> %s\n\n" (sprint_all (x,c)) at
      (print_tab (eval at x) (fun z -> sprint_terms (assemble (z,c)) ))
    ) l *)



(* let () = List.iter (fun i -> Printf.printf "%s\n" (sprint_terms i)) (all_eval t) *)
(* let () = Printf.printf "%s\n" (sprint_terms t) *)

let () = Printf.printf "%s\n" (sprint_tree "" (full_eval t))




let file = "treeData.json"
let message = Printf.sprintf "[%s]\n" (sprint_tree_json "null" (full_eval t))

let () =
  let oc = open_out file in
  fprintf oc "%s\n" message;
  close_out oc;
