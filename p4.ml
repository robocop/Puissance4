open Extendn;;
open Board;;
open Eval;;

let rec minmax prof = 
  let moves = Board.get_moves () in  (* on recupère tous les coups valides. *) 
  let rec foreach_moves (best_score, best_move)= function
    | [] ->  (best_score, best_move)  (* on renvoit le meilleur coup trouvé. *)
    | m::list ->  (* pour chaque coup de la liste *)
      Board.move m;  (* on le joue *)
	let score = eval () in
	  if score = PInf then (Board.cancel(); (score, m)) (* s'il permet de gagner directement, on le choisit ! *)
	  else
	    let s, _ = if prof <= 0 then ((--)(eval ()), m) else minmax (prof-1) in  (* sinon on regarde les réplique de l'adversaire*)
	      Board.cancel (); (* on annule le coup et on passe au suivant. *)
	      foreach_moves (if ((--) s) >>= best_score then ((--) s, m) else  (best_score, best_move)) list
  in  
    if moves = [] then (N 0, 0) (* si il y a aucun coup à jouer alors il y a match nul : score = 0. *)
    else
      foreach_moves (MInf, 0) moves (* on parcours les coups et on cherche celui qui rapporte le plus de points *)
;; 

let _ = 
  let couleur_joueur = R in
  Board.init();
  let rec loop() = 
    Board.print ();
    match Board.get_moves() with
      | [] -> print_endline "done";
      | l ->
	  if couleur_joueur = Board.turn() then 
	    let coup = read_int () in
	      if not (List.mem coup l) then loop ()
	      else
		(Board.move coup; loop ())
	  else
	    let coup = snd (minmax 5) in
	    (Board.move coup; loop())
  in
    loop ()
;;

