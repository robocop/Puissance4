(* open Types;; *)
(* open Extendn;; *)
(* open Eval;; *)

let move b x = 
  let y = b.colones.(x) in
    b.board.(x).(y) <- b.whos_turn;
    b.colones.(x) <- y+1;
    {b with whos_turn = !!(b.whos_turn); moves = (x, y)::b.moves}
;;

let cancel b = 
  let x, y = List.hd b.moves in
    b.colones.(x) <- b.colones.(x) -1;
    b.board.(x).(y) <- E;
    {b with whos_turn = !!(b.whos_turn); moves = List.tl b.moves }
;; 
let make_cancel b = ignore (cancel b);;

let print_board board = 
  let separator = "\n   +----+----+----+----+----+----+----+\n" in
    print_string separator;
    for j = 5 downto 0 do
      Printf.printf " %d |" (j);
      for i = 0 to 6 do
        match board.board.(i).(j) with
          | R -> Printf.printf "  R |" 
	  | Y -> Printf.printf "  Y |"
          | E -> print_string "    |"
      done;
      print_string separator;
    done;
    print_string "\n      0    1    2    3    4    5    6\n"

;;


let get_valids_moves board = 
  let l = ref [] in
    for i = 0 to 6 do
      if board.colones.(i) < 6 then l := i::!l;
    done;
    !l
;;


let print b = 
  print_endline (string_of_score (eval b));
  print_board b
;;


let rec minmax b prof = 
  let moves = get_valids_moves b in  (* on recupère tous les coups valides. *) 
  let rec foreach_moves (best_score, best_move)= function
    | [] ->  (best_score, best_move)  (* on renvoit le meilleur coup trouvé. *)
    | m::list ->  (* pour chaque coup de la liste *)
	let b = move b m in  (* on le joue *)
	let score = eval b in
	  if score = PInf then (make_cancel b; (score, m)) (* s'il permet de gagner directement, on le choisit ! *)
	  else
	    let s, _ = if prof <= 0 then ((--)(eval b), m) else minmax b (prof-1) in  (* sinon on regarde les réplique de l'adversaire*)
	      make_cancel b; (* on annule le coup et on passe au suivant. *)
	      foreach_moves (if ((--) s) >>= best_score then ((--) s, m) else  (best_score, best_move)) list
  in  
    if moves = [] then (N 0, 0) (* si il y a aucun coup à jouer alors il y a match nul : score = 0. *)
    else
      foreach_moves (MInf, 0) moves (* on parcours les coups et on cherche celui qui rapporte le plus de points *)
;; 

let _ = 
  let couleur_joueur = Y in
  let b = {whos_turn = R; moves = []; colones = Array.make 7 0; board = Array.create_matrix 7 6 E} in
  let rec loop b = 
    print b;
    match get_valids_moves b with
      | [] -> print_endline "done";
      | l ->
	  if couleur_joueur = b.whos_turn then 
	    let coup = read_int () in
	      if not (List.mem coup l) then loop b
	      else
		loop (move b coup)
	  else
	    let coup = snd (minmax b 1) in
	      loop (move b coup)
  in
    loop b
;;

