(* Un objet : son poids et sa valeur *)
type item = { weight : int; value : int }

(* Une instance : la capacité du sac et la liste des objets *)
type t = { capacity : int; items : item array }

let print (inst : t) =
  Printf.printf "capacite = %d, %d objets\n" inst.capacity (Array.length inst.items);
  Array.iteri
    (fun i it -> Printf.printf "  objet %d : poids %d, valeur %d\n" i it.weight it.value)
    inst.items