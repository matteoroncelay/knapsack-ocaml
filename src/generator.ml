open Instance

type famille = Non_correlee | Faible | Forte

let nom_famille (f : famille) : string =
  match f with
  | Non_correlee -> "Non correlee"
  | Faible -> "Faiblement correlee"
  | Forte -> "Fortement correlee"

let generator (poids_max : int) (val_add : int) (capacite_max : int) (nb_objet : int) (typ : famille) : t =
  let objets = Array.make nb_objet { weight = 0; value = 0 } in
    for i = 0 to (nb_objet-1) do
      match typ with
      | Non_correlee -> begin
        objets.(i) <- {weight = 1 + Random.int poids_max ; value = 1 + Random.int val_add}end
      | Faible -> begin
        let poids = 1 + Random.int poids_max in
        objets.(i) <- {weight = poids ; value = max (poids + 1 + Random.int val_add - (val_add/2)) 1}end
      | Forte -> begin
        let poids = 1 + Random.int poids_max in
        objets.(i) <- {weight = poids ; value = poids + val_add}end
    done;
    {capacity = Random.int (capacite_max+1); items=objets}


