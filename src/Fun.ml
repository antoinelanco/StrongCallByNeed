open Type
open Aux

module Phi = Set.Make(String)

module Context = Set.Make(
  struct
    let compare = Pervasives.compare
    type t = full_context
  end)



let rec is_struct = function
  | Lambda _ -> None
  | App (t1,_) -> is_struct t1
  | Var x -> Some x
  | ES (t1,x,t2) ->
    begin
      match is_struct t1 with
      | Some x' when x' = x -> is_struct t2
      | a -> a
    end


let context inert w =
  let rec aux phi at c = function
    | Lambda (x,t) ->
      let new_phi = if at then phi else (Phi.add x phi) in
      aux new_phi at (C_Lambda x::c) t

    | App (t1,t2) ->

      let r = Context.add (App(t1,t2),c,at) (aux phi true (C_AppG t2::c) t1) in
      begin
        match is_struct t1 with
        | Some x when Phi.exists ((=)x) phi ->
          Context.union r (aux phi false (C_AppD t1::c) t2)
        | _ -> r
      end

    | Var (x) -> Context.singleton (Var x,c,at)



    | ES (t1,x,t2) ->

      let new_t1 = aux phi at (C_ESG (x,t2)::c) t1 in

      let c1 =
        if Context.exists (fun (y,_,_) -> y = (Var x)) new_t1
        then aux phi true (C_ESD (t1,x)::c) t2
        else Context.empty in

      let c2 =
        begin
          match is_struct t2 with
          | Some x' when Phi.exists ((=)x') phi ->
            aux (Phi.add x phi) at (C_ESG (x,t2)::c) t1
          | _ -> new_t1
        end in

      Context.add (ES (t1,x,t2),c,at) (Context.union c1 c2)



  in
  let r = aux Phi.empty inert [C_HOLE] w in
  Context.map (fun (a,b,at) -> (a,List.rev b,at)) r


let rec find_lambda c = function
  | Lambda(v,t) -> Some (c,Lambda(v,t))
  | ES(t1,x,t2) -> find_lambda (fun a -> c (ES(a,x,t2)) ) t1
  | _ -> None


let rec assemble (t,cl) =

  List.fold_right (fun i acc ->
      match i with
      | C_Lambda v -> Lambda(v,acc)
      | C_AppG t' -> App(acc,t')
      | C_AppD t' -> App(t',acc)
      | C_ESG (v,t') -> ES(acc,v,t')
      | C_ESD (t',v) -> ES(t',v,acc)
      | C_HOLE -> acc
    ) cl t



let rec eval at = function
  | App(t1,t2) ->
    let r =
      begin
        match find_lambda (fun i -> i) t1 with
        | Some (f,Lambda(x,t)) -> f (ES(t,x,t2))
        | _ -> App(t1,t2)
      end in
    [r]

  | ES(t1,x,t2) -> (*Trouver [x] savoir si v est NF rempalcer [x] extraire L*)


    (*t2 is val ??*)
    let is_val = find_lambda (fun i -> i) t2 in
    begin
      match is_val with
      | Some (cont,term) when is_nf true term -> (*t2 is NF*) (* !!!!!!!!!!!!! *)

        (*eval all var*)

        let c_t1 = context false t1 in
        let context_filter = Context.filter (fun (i,_,_) -> i = Var x ) c_t1 in
        if Context.is_empty context_filter
        then [ES(t1,x,t2)]
        else (*Check t2 ∈ NF + Non-Determinisme*)
          List.map (fun (_,c,_) ->
              cont (ES(assemble (term, c),x,term)))
            (Context.elements context_filter) (*ND*)

      | _ -> [ES(t1,x,t2)] (*Pas val => pas de sub*)

    end


  | x -> [x]


and is_nf at term =
  let cont = context at term in
  let list_eval = Context.fold (fun (i,c,at) acc ->
      (List.map (fun z -> assemble(z,c) ) (eval at i)) @ acc) cont [] in

  not (List.exists (fun i -> i <> term) list_eval)


let all_eval t =
  let cont = context false t in
  let list_eval = Context.fold (fun (i,c,at) acc ->
      (List.map (fun z -> assemble(z,c) ) (eval at i)) @ acc) cont [] in
  List.filter (fun i -> i <> t) list_eval


let rec full_eval n t =
  if n <= 0 then Leaf else
    let (t,_) = clear Bound.empty t in
    Node (alpha t,List.map (fun i -> full_eval (n-1) i) (all_eval t))
    (* Node (t,List.map (fun i -> full_eval (n-1) i) (all_eval t)) *)
