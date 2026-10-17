open Knapsack.Instance
open Knapsack.Generator

(* Unix.gettimeofday : precision a la microseconde, contrairement a Sys.time sous Windows *)
let chrono (f : 'a -> 'b) (x : 'a) : 'b * float =
  let debut = Unix.gettimeofday () in
  let resultat = f x in
  let fin = Unix.gettimeofday () in
  (resultat, fin -. debut)

(* Capacite standard : la moitie de la somme des poids *)
let avec_capacite_standard (inst : t) : t =
  let somme = Array.fold_left (fun acc it -> acc + it.weight) 0 inst.items in
  { inst with capacity = somme / 2 }

(* Parametre de valeur adapte a chaque famille *)
let val_add (f : famille) : int =
  match f with
  | Non_correlee -> 100   (* valeur entre 1 et 100, independante du poids *)
  | Faible -> 20          (* valeur = poids + environ -10 a +10 *)
  | Forte -> 10           (* valeur = poids + 10 *)

(* Tailles mesurees : la famille forte explose bien plus tot pour le B&B *)
let tailles (f : famille) : int list =
  match f with
  | Non_correlee | Faible -> [100; 200; 500; 1000; 2000]
  | Forte -> [10; 20; 30; 40; 50; 60; 70; 80; 90; 100]

(* Numero de famille, pour fabriquer une graine propre a chaque point de mesure *)
let numero (f : famille) : int =
  match f with
  | Non_correlee -> 1
  | Faible -> 2
  | Forte -> 3

let poids_max = 100
let nb_instances = 10

(* Pour une famille et un nombre d'objets : moyennes sur nb_instances instances *)
let mesure (f : famille) (n : int) =
  (* graine fixe pour ce point : memes instances quelles que soient les mesures faites avant *)
  Random.init (100_000 * numero f + n);
  let total_dp = ref 0. and total_bnb = ref 0. in
  let total_noeuds = ref 0 and total_ecart = ref 0. in
  for _i = 1 to nb_instances do
    let inst = avec_capacite_standard (generator poids_max (val_add f) 0 n f) in
    let (opt_dp, t_dp) = chrono Knapsack.Dynprog.solve inst in
    let ((opt_bnb, noeuds), t_bnb) = chrono Knapsack.Bnb.solve_stat inst in
    let glouton = Knapsack.Greedy.solve inst in
    if opt_dp <> opt_bnb then failwith "B&B et programmation dynamique en desaccord";
    total_dp := !total_dp +. t_dp;
    total_bnb := !total_bnb +. t_bnb;
    total_noeuds := !total_noeuds + noeuds;
    if opt_dp > 0 then
      total_ecart := !total_ecart
                     +. 100. *. float_of_int (opt_dp - glouton) /. float_of_int opt_dp
  done;
  let k = float_of_int nb_instances in
  (!total_dp /. k, !total_bnb /. k, float_of_int !total_noeuds /. k, !total_ecart /. k)

let () =
  let canal = open_out "resultats_a.csv" in
  Printf.fprintf canal "famille,nb_objets,temps_dp,temps_bnb,noeuds_bnb,ecart_glouton\n";
  List.iter
    (fun f ->
       List.iter
         (fun n ->
            let (t_dp, t_bnb, noeuds, ecart) = mesure f n in
            Printf.fprintf canal "%s,%d,%.6f,%.6f,%.1f,%.2f\n%!"
              (nom_famille f) n t_dp t_bnb noeuds ecart;
            Printf.printf "%s, %d objets : dp %.4f s, bnb %.4f s, %.0f noeuds, glouton -%.2f %%\n%!"
              (nom_famille f) n t_dp t_bnb noeuds ecart)
         (tailles f))
    [Non_correlee; Faible; Forte];
  close_out canal