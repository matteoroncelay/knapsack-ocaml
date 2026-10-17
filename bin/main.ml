open Knapsack.Instance

let test nom inst =
  Printf.printf "%s : capacite %d, %d objets, glouton = %d, dynamique = %d, bnb = %d\n"
    nom inst.capacity (Array.length inst.items)
    (Knapsack.Greedy.solve inst)
    (Knapsack.Dynprog.solve inst)
    (Knapsack.Bnb.solve inst)

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
  let inst3 = {
    capacity = 5;
    items = [| { weight = 2; value = 9 };
               { weight = 6; value = 6 };
               { weight = 4; value = 3 } |]
  } in
  let inst4 = {
    capacity = 2;
    items = [| { weight = 1; value = 2 } |]
  } in
  let inst5 = {
    capacity = 10;
    items = [| { weight = 6; value = 12 };
               { weight = 5; value = 5 };
               { weight = 4; value = 3 } |]
  } in
  test "test1" inst;
  test "test2" inst2;
  test "test3" inst3;
  test "test4" inst4;
  test "test4" inst5