open Knapsack.Instance

let test nom inst =
  Printf.printf "%s : capacite %d, %d objets, glouton = %d\n"
    nom inst.capacity (Array.length inst.items)
    (Knapsack.Greedy.solve inst)

let () =
  let inst = {
    capacity = 10;
    items = [| { weight = 5; value = 10 };
               { weight = 4; value = 7 };
               { weight = 3; value = 4 } |]
  } in
  let inst2 = {
    capacity = 10;
    items = [| { weight = 6; value = 7 };
               { weight = 5; value = 5 };
               { weight = 5; value = 5 } |]
  } in
  test "exemple" inst;
  test "contre-exemple" inst2