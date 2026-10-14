open Instance

let solve (inst : t) : int =
  let n = Array.length inst.items in
  let cap = inst.capacity in
  let table = Array.make_matrix (n + 1) (cap + 1) 0 in
  for i = 1 to n do
    let it = inst.items.(i - 1) in
    for c = 0 to cap do
      if (c>= it.weight) then table.(i).(c) <- (max (table.(i-1).(c)) (table.(i-1).(c-it.weight)+it.value))
      else table.(i).(c) <- (table.(i-1).(c))
    done
  done;
  table.(n).(cap)