(* Un objet : son poids et sa valeur *)
type item = { weight : int; value : int }

(* Une instance : la capacité du sac et la liste des objets *)
type t = { capacity : int; items : item array }