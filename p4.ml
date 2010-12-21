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

let (!!) = function R -> Y | Y -> R;;

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

 let count b (x, y) (i, j) = 
   let piece = b.board.(x).(y) in
   let rec count (x, y) = 
     if not (in_board (x, y)) then 0
     else
       match b.board.(x).(y) with
	 | p when p = piece -> 1 + count (x+i, y+j)
	 | _ -> 0
   in
   count (x+i, y+j)
;;

 let is_win b =
   let x, y = List.hd (b.moves) in
     count b (x, y) (1,0) + count b (x, y) (-1,0) >= 3 
  || count b (x, y) (0,1) + count b (x, y) (0,-1) >= 3 
  || count b (x, y) (1,1) + count b (x, y) (-1,-1) >= 3 
  || count b (x, y) (-1,1) + count b (x, y) (1,-1) >= 3 
;;

(* tests... *)
 let ib = {whos_turn = R; moves = []; colones = Array.make 7 0; board = Array.create_matrix 7 6 E};;
 let b = move ib 3;;
 let b = move b 4;;
 let b = move b 1;;
 print_board b;

count b (3,1) (1,0);;
is_win b;;

