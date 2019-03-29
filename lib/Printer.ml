open Printf

let print_tab tab f =
  let chaine = String.concat ";" (List.map f tab) in
  printf "[%s]\n" chaine
