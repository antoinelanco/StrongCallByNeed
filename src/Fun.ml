open Type

module Phi = Set.Make(String)

module Context = Set.Make(
  struct
    let compare = Pervasives.compare
    type t = pair
  end)



let rec is_struct = function
  | T_Lambda _ -> None
  | T_App (t1,_) -> is_struct t1
  | T_Var x -> Some x
  | T_ES (t1,x,t2) ->
    begin
      match is_struct t1 with
      | Some x' when x' = x -> is_struct t2
      | a -> a
    end


let context w =
  let rec aux phi at c = function
    | T_Lambda (x,t) ->
      let new_phi = if at then phi else (Phi.add x phi) in
        aux new_phi at (C_Lambda x::c) t

    | T_App (t1,t2) ->



      let r = Context.add (T_App(t1,t2),c) (aux phi true (C_AppG t2::c) t1) in
      begin
        match is_struct t1 with
        | Some x when Phi.exists ((=)x) phi ->
            Context.union r (aux phi false (C_AppD t1::c) t2)
        | _ -> r
      end

    | T_Var (x) -> Context.singleton (T_Var x,c)



    | T_ES (t1,x,t2) ->

      let new_t1 = aux phi at (C_ESG (x,t2)::c) t1 in

      let c1 =
        if Context.exists (fun (y,_) -> y = (T_Var x)) new_t1
        then aux phi true (C_ESD (t1,x)::c) t2
        else Context.empty in

      let c2 =
        begin
          match is_struct t2 with
          | Some x' when Phi.exists ((=)x') phi ->
              aux (Phi.add x phi) at (C_ESG (x,t2)::c) t1
          | _ -> new_t1
        end in

      Context.add (T_ES (t1,x,t2),c) (Context.union c1 c2)



  in
  let r = aux Phi.empty false [C_HOLE] w in
  Context.map (fun (a,b) -> (a,List.rev b)) r




let eval = function
  | T_App(T_Lambda(x,t1),t2) -> T_ES(t1,x,t2)
  (* | ES(t1,x,t2) -> *)
  | x -> x
