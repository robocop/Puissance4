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

 let ib = {whos_turn = R; moves = []; colones = Array.make 7 0; board = Array.create_matrix 7 6 E};;
 let b = move ib 3;;
 let b = move b 4;;
 let b = cancel b;;
 print_board b;
