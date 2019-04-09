open Type

module Bound = Set.Make(String)

let new_id =
  let r = ref "x" in
  fun () -> r := !r^"'"; !r


let rec replace o n = function
  | Lambda(x,t) -> let new_x = if x = o then n else x in
    Lambda(new_x,replace o n t)
  | App(t,u) -> App(replace o n t,replace o n u)
  | Var x -> Var (if x = o then n else x)
  | ES(t,x,u) ->
    let new_x = if x = o then n else x in
    ES(replace o n t,new_x,replace o n u)


let rec get_bounded_var = function
  | Lambda(x,t) -> Bound.add x (get_bounded_var t)
  | App(t,u) -> Bound.union (get_bounded_var t) (get_bounded_var u)
  | Var _ -> Bound.empty
  | ES(t,x,u) -> Bound.add x (Bound.union (get_bounded_var t) (get_bounded_var u))


let alpha terms =
  let r = ref 65 in
  let rec aux env term =
    let s = Printf.sprintf "%c" (char_of_int !r) in
    match term with
    | Lambda(x,t) ->
      if Bound.exists ((=)x) env
      then Lambda(x,aux env t)
      else
        let () = incr r in
        let t' = replace x s t in
        let env' = Bound.add s env in
        Lambda(s,aux env' t')

    | App(t,u) -> App(aux env t,aux env u)

    | Var x ->
      if Bound.exists ((=)x) env
      then Var x
      else let () = incr r in Var s

    | ES(t,x,u) ->
      if Bound.exists ((=)x) env
      then ES(aux env t,x,aux env u)
      else
        let () = incr r in
        let t' = replace x s t in
        let env' = Bound.add s env in
        ES(aux env' t',s,aux env' u)

  in
  aux Bound.empty terms


let rec clear env = function
  | Lambda(x,t) ->
    let new_x = if Bound.exists ((=)x) env then new_id () else x in
    let (t',env') = clear (Bound.add new_x env) (replace x new_x t) in
    (Lambda(new_x,t'),env')

  | App(t,u) ->
    let (t',env') = clear env t in
    let (t'',env'') = clear env' u in
    (App(t',t''),env'')

  | Var x -> (Var x,env)

  | ES(t,x,u) ->
    let (t',env') = clear env t in
    let (t'',env'') = clear env' u in
    (ES(t',x,t''),env'')
