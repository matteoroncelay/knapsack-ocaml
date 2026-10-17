open Instance

let ratio (it : item) : float =
  float_of_int it.value /. float_of_int it.weight


let rec recursif (poids_restant : int) (sorted : item array) (indice : int) : int*item list =
  if (indice>=Array.length sorted) then (0,[]) else
  if (sorted.(indice).weight>poids_restant) 
    then recursif (poids_restant) sorted (indice+1)
  else let (valeur, liste) = recursif (poids_restant-sorted.(indice).weight) sorted (indice+1) in
    (valeur+sorted.(indice).value,sorted.(indice)::liste)
let solve (inst : t) : int =
  let sorted = Array.copy inst.items in
  Array.sort (fun x y -> compare (ratio y) (ratio x)) sorted; 
  let (valeur,_) = recursif (inst.capacity) sorted 0 in 
  valeur