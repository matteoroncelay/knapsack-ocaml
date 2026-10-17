open Knapsack.Generator

let rec recursion (temp : int) (capacite_max : int) (poids_max : int) (val_max : int) (nb_objet_max : int) (typ : Knapsack.Generator.famille)=
  if (temp<=0) then ()
  else
    let nb_objet = Random.int (nb_objet_max+1) in
    let inst = (Knapsack.Generator.generator poids_max val_max capacite_max nb_objet typ) in
    let val1 = Knapsack.Bnb.solve inst in
    let val2 = Knapsack.Dynprog.solve inst in
    let val3 = Knapsack.Greedy.solve inst in
    match val1=val2 with
    | true -> if (val3>val2) then begin
            print_endline "ECHEC : resultats incoherent sur cette instance";
            Knapsack.Instance.print inst;
            failwith (Printf.sprintf "programmation dynamique = %d, algo glouton = %d" val2 val3)
          end
        else recursion (temp-1) capacite_max poids_max val_max nb_objet_max typ
    | false -> begin
        print_endline "ECHEC : resultats differents sur cette instance";
        Knapsack.Instance.print inst;
        failwith (Printf.sprintf "branch & bound = %d, programmation dynamique = %d" val1 val2)
      end


let () =
  Random.self_init ();
  let graine = Random.int 1_000_000 in
  Printf.printf "Graine : %d\n" graine;
  Random.init graine;
  List.iter
    (fun famille ->
       recursion 100000 20 10 10 5 famille;
       Printf.printf "%s : 100000 petites instances testees (jusqu'a 5 objets), aucun desaccord\n"
         (nom_famille famille);
       recursion 10000 40 10 10 10 famille;
       Printf.printf "%s : 10000 grandes instances testees (jusqu'a 10 objets), aucun desaccord\n"
         (nom_famille famille))
    [Non_correlee; Faible; Forte]