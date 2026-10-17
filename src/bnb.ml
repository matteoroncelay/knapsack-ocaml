open Instance

let ratio (t : item) : float =
  float_of_int t.value /. float_of_int t.weight
let trie (inst : t) : item array=
  let sorted = Array.copy inst.items in Array.sort (fun x y -> compare (ratio y) (ratio x)) sorted;
  sorted

let rec borne (val_actuel : int) (poid_restant : int) (indice : int) (sorted : item array) : float =
  let rep = float_of_int val_actuel in
  if (indice >= Array.length sorted) then rep else
  if (poid_restant>0) then
    if (poid_restant>=sorted.(indice).weight) then
      borne (val_actuel+sorted.(indice).value) (poid_restant-sorted.(indice).weight) (indice+1) (sorted)
    else
      let ratio_reste = (ratio sorted.(indice)) in rep+.(ratio_reste*.float_of_int (poid_restant))
  else rep


let max_solve (valeur1,liste1,compt1) (valeur2,liste2,compt2) : (int* 'a*int) =
  if(valeur1<valeur2) then (valeur2,liste2,compt2) else (valeur1,liste1,compt1)
let rec recursif (meilleure_solve : int) (sorted : item array) (indice : int) (val_actuel: int) (poid_restant : int) (compt : int): (int*item list*int) =
  if (poid_restant=0 || indice >= Array.length sorted) then (val_actuel,[],compt) 
  else
    let born = borne val_actuel poid_restant indice sorted in 
    if (born<= float_of_int meilleure_solve) then (0,[],compt) 
    else
      if (sorted.(indice).weight>poid_restant) then recursif meilleure_solve sorted (indice+1) val_actuel poid_restant (compt+1)
      else
        let (val1,liste1,compt1) = recursif meilleure_solve sorted (indice+1) (val_actuel+sorted.(indice).value) (poid_restant-sorted.(indice).weight) (compt+1) in
        let (val2,liste2,compt2) = recursif (max val1 meilleure_solve) sorted (indice+1) (val_actuel) (poid_restant) (compt1+1) in
        (max_solve (val1,(sorted.(indice)::liste1),compt2) (val2,liste2,compt2))

let solve_stat (inst : t) : int*int =
  let sorted = trie inst in
  let (valeur1) = Greedy.solve inst in
  let (valeur2, _, compt) = recursif valeur1 sorted 0 0 inst.capacity 0 in
  (max valeur1 valeur2),compt

let solve (inst : t) : int =
  let (valeur,_) = solve_stat inst in
  valeur