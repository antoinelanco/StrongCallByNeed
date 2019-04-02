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

let t = Lambda("x",App(Var "x",Var "x"))

let t = Lambda("a",ES(Lambda("x",App(Var "x", Var "x")),"x",id "y"))

let t = ES(Var "x","x",Lambda("y",App(Var "y",App(id "a",Var "z"))))

let t = ES(Lambda("y",App(App(Var "y", Var "x"), Var "x")),"x",id "a")

(* let t = ES(Var "x","x",ES(id "z","a",Var "e")) *)

(* let () = Printf.printf "%s : \n\n" (sprint_terms t)

let l = context false t
let () = Context.iter (fun (x,c,at) ->
    Printf.printf "%s:%b -> %s\n\n" (sprint_all (x,c)) at
      (print_tab (eval at x) (fun z -> sprint_terms (assemble (z,c)) ))
    ) l *)

(* let t = Var "x" *)







let file = "treeData.json"
let message = Printf.sprintf "[%s]\n" (sprint_tree "null" (full_eval t))

let () =

  let oc = open_out file in
  fprintf oc "%s\n" message;
  close_out oc;

  let ic = open_in file in
  let line = input_line ic in  (* read line from in_channel and discard \n *)
  print_endline line;          (* write the result to stdout *)
  flush stdout;                (* write on the underlying device now *)
  close_in ic                  (* close the input channel *)
