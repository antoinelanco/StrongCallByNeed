module G = Set.Make(
  struct
    let compare = Pervasives.compare
    type t = terms * terms list
  end)


let tree_to_graph graph = function
  | Leaf -> graph
  | Node(t,l) ->
    let g = in
    let l' = List.fold_left (fun acc i -> ... ) [] l in
    G.add (t,l') g
