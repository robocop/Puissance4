(* Extension de N : ajout de +oo et -oo pour le gain et la défaite. *)
module ExtendN = struct
  type score = PInf | N of int | MInf
  let (--) = function
    | PInf -> MInf
    | MInf -> PInf
    | N n -> N (-n)

  let (>>=) a b = match (a, b) with
    | PInf, MInf -> true
    | MInf, PInf -> false
    | PInf, PInf | MInf, MInf -> true
    | MInf, N n -> false
    | N n, MInf -> true
    | PInf, N n -> true
    | N n, PInf -> false
    | N n, N m -> n >= m
  let (>>) a b = 
    a >>= b && a <> b
  let string_of_score = function
    | MInf -> "-oo"
    | PInf -> "+oo"
    | N n -> string_of_int n;;
end;; 
open ExtendN;;

type case = R | Y | E;;
type board = 
    {
      whos_turn:case;
      moves:(int*int) list;
      colones:int array;
      board:case array array;
    }
;;

let (!!) = function R -> Y | Y -> R | E -> E;;

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

let in_board (x, y) = 
  x >= 0 && y >= 0 && x < 7 && y < 6
;;

let spec_count b (x,y) (i,j) piece = 
  let rec count (x, y) = 
    if not (in_board (x, y)) then 0
    else
      match b.board.(x).(y) with
	| p when p = piece -> 1 + count (x+i, y+j)
	| _ -> 0
  in count (x+i, y+j)
;;

let count b (x, y) (i, j) = 
  let piece = b.board.(x).(y) 
  in spec_count b (x,y) (i,j) piece
;;

let who_won b =
  try 
    let x, y = List.hd (b.moves) in
      if count b (x, y) (1,0) + count b (x, y) (-1,0) >= 3 
	|| count b (x, y) (0,1) + count b (x, y) (0,-1) >= 3 
	|| count b (x, y) (1,1) + count b (x, y) (-1,-1) >= 3 
	|| count b (x, y) (-1,1) + count b (x, y) (1,-1) >= 3 
      then b.board.(x).(y)
      else E
  with _ -> E
;;

let get_valids_moves board = 
  let l = ref [] in
    for i = 0 to 6 do
      if board.colones.(i) < 6 then l := i::!l;
    done;
    !l
;;

(* EVAL *)

let all_directions = [(1,0);(-1,0);(0,1);(0,-1);(1,1);(-1,-1);(-1,1);(1,-1)];;
let oneway_dirs = [(1,0);(0,1);(1,1);(-1,1)];;

let (+++) (x,y) (x', y') = (x+x', y+y');;
let (---) a (x, y) = a +++ (-x, -y);;

(* returns a block witrh it's starting point, end point, and direction *)
let block b (x,y) dir = 
  let t = b.board.(x).(y) 
  in
  let rec aux f (x,y) =
    let (x',y') = f (x,y) dir
    in if in_board (x', y') && b.board.(x').(y') = t 
      then aux f (x', y')
      else (x,y)
  in (aux (---) (x,y), aux (+++) (x,y), dir)
;;

(* says if pt is in block *)
let in_block (x,y) ((minx, miny), (maxx, maxy), _) = 
  minx <= x && x <= maxx && miny <= y && y <= maxy
;;

let length_block b (min, _, dir) = count b min dir + 1;;

(* make_blocks from board moves *)
let make_blocks b = 
  let aux acc e = (List.map (block b e) oneway_dirs)@acc
  in List.fold_left aux [] b.moves
;;

let fits b p (x,y) (u,v) n =
  let rec fits' (x,y) = 
    if in_board (x,y) && (b.board.(x).(y) = E || b.board.(x).(y) = p)
    then 1 + fits' (x+u, y+v)
    else 0
  in fits' (x,y) >= n
;;

let rec heights b (x,y) (u,v) player = function 
  | n when n<=0 -> 0
  | n -> match b.board.(x).(y) with
      | E -> 1 + spec_count b (x,y) (0,-1) E + heights b (x+u, y+v) (u,v) player (n-1)
      | p when p = player -> heights b (x+u, y+v) (u,v) player (n-1)
      | _ -> failwith "Hit unrecheable code in heights"
;;

let from b ((x,y) as pos) dir n = 
  let player = b.board.(x).(y)
  in if fits b player (pos+++dir) dir n (* if n empty or already owned squares follow the current one *)
    then heights b (pos+++dir) dir player n (* add up all the empty squares (+ their height) that follow pos *)
    else 9999 (* Replace with extendn eventually *)
;;

let from_top = from;;
let from_bottom a b dir e = from a b ((0,0) --- dir) e;; 

(* returns the minimal number of moves before the block becomes valid in a certain direction *)
(* bottom = any pt in block --- dir a few times, top = any pt in block +++ dir a few times*)
let block_outcome b ((bottom, top, dir) as block) = 
  let elem_left = 4 - length_block b block in
  let t = from_top b top dir elem_left in
  let b = from_bottom b bottom dir elem_left
  in match (t,b) with
    | 9999, 9999 -> 0
    | 9999, _    -> b
    | _, 9999    -> t
    | _          -> min t b
;;

let block_player b ((x,y), _, _) = 
  b.board.(x).(y)
;;

let do_eval b player = 
  let aux (r, y, rpossibilities, ypossibilities) block = 
    match block_player b block with
      | R -> (r + block_outcome b block, y, rpossibilities+1, ypossibilities)
      | Y -> (r, y + block_outcome b block, rpossibilities, ypossibilities+1)
      | E -> failwith "Cannot have a block of empties"
  in 
  let blocks = make_blocks b (* TODO : make sure they are all different *)
  in match List.fold_left aux (0,0,0,0) blocks with
      (* The one that has the smallest score is winning, therefore : *)
    | (r,y, rp, yp) -> if player = R
      then (float y)/.(float yp) -. (float r)/.(float rp) 
      else (float r)/.(float rp) -. (float y)/.(float yp) 
	(* In the output, bigger score is better *)
;;

let generic_eval b player f = 
  if who_won b = E 
  then N (int_of_float (f b player))
  else if who_won b = player 
  then PInf
  else MInf
;;

let eval b = 
  generic_eval b (b.whos_turn) do_eval
;;
(* END EVAL *)

let print b = 
  print_endline (string_of_score (eval b));
  print_board b
;;

let rec minmax b prof = 
  let moves = get_valids_moves b in
  let rec foreach_moves (best_score, best_move)= function
    | [] ->  (best_score, best_move)
    | m::list ->
	let b = move b m in
	let score = eval b in
	  if score = PInf then (make_cancel b; (score, m))
	  else
	    let s, _ = if prof <= 0 then ((--)(eval b), m) else minmax b (prof-1) in
	      make_cancel b;
	      foreach_moves (if ((--) s) >>= best_score then ((--) s, m) else  (best_score, best_move)) list
  in  
    if moves = [] then (N 0, 0)
    else
      foreach_moves (MInf, 0) moves
;; 

let _ = 
  let couleur_joueur = R in
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
	    let coup = snd (minmax b 4) in
	      loop (move b coup)
  in
    loop b
;;

(*
let ib = {whos_turn = R; moves = []; colones = Array.make 7 0; board = Array.create_matrix 7 6 E};;
let b = move ib 1;;
print_board b;;
minmax b 3;;
let b = move ib 3;;
let b = move b 4;;
let b = move b 4;;
get_valids_moves b;;
print_board ib;;

count b (3,1) (1,0);;
who_won b;;

*)
