open Instance

let ratio (it : item) : float =
  float_of_int it.value /. float_of_int it.weight

let solve (inst : t) : int =
  let sorted = Array.copy inst.items in
  Array.sort (fun x y -> compare (ratio y) (ratio x)) sorted; 
  let (_, valeur) =
    Array.fold_left
      (fun (cap, v) it ->
         if it.weight <= cap then (cap - it.weight, v + it.value)
         else (cap, v))
      (inst.capacity, 0)
      sorted
  in
  valeur