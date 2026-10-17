open Instance

let generator (poids_max : int) (val_max : int) (capacite : int) (nb_objet : int) : t =
  let objets = Array.make nb_objet { weight = 0; value = 0 } in
  for i = 0 to (nb_objet-1) do
    objets.(i) <- {weight = 1 + Random.int poids_max ; value = 1 + Random.int val_max}
  done;
  {capacity = capacite; items = objets}

