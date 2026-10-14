open Knapsack.Instance

let () =
  let inst = {
    capacity = 10;
    items = [| { weight = 5; value = 10 };
               { weight = 4; value = 7 };
               { weight = 3; value = 4 } |]
  } in
  Printf.printf "Capacite : %d, nombre d'objets : %d\n"
    inst.capacity (Array.length inst.items);
  Printf.printf "Glouton : %d\n" (Knapsack.Greedy.solve inst)