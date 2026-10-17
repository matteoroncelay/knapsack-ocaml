let rec recursion (temp : int) (capacite_max : int) (poids_max : int) (val_max : int) (nb_objet_max : int)=
  if (temp<=0) then ()
  else
    let capacite = Random.int (capacite_max+1) in
    let nb_objet = Random.int (nb_objet_max+1) in
    let inst = (Knapsack.Generator.generator poids_max val_max capacite nb_objet) in
    let val1 = Knapsack.Bnb.solve inst in
    let val2 = Knapsack.Dynprog.solve inst in
    let val3 = Knapsack.Greedy.solve inst in
    match val1=val2 with
    | true -> if (val3>val2) then begin
            print_endline "ECHEC : resultats incoherent sur cette instance";
            Knapsack.Instance.print inst;
            failwith (Printf.sprintf "programmation dynamique = %d, algo glouton = %d" val2 val3)
          end
        else recursion (temp-1) capacite_max poids_max val_max nb_objet_max
    | false -> begin
        print_endline "ECHEC : resultats differents sur cette instance";
        Knapsack.Instance.print inst;
        failwith (Printf.sprintf "branch & bound = %d, programmation dynamique = %d" val1 val2)
      end


let ()=
  Random.self_init ();
  let graine = Random.int 1_000_000 in
  Printf.printf "Graine : %d\n" graine;
  Random.init graine;
  recursion 100000 20 10 10 5;
  Printf.printf "recursion ~tour:100000 ~capacitemax:20 ~poidsmax:10 ~valmax:10 ~nbovjetmax:5 reussi\n";
  recursion 10000 40 10 10 10;
  Printf.printf "recursion tour:10000 ~capacitemax:40 poidsmax:10 ~valmax:10 ~nbovjetmax:10 reussi\n"