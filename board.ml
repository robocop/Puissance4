open Extendn
type case = R | Y | E
let (!!) = function R -> Y | Y -> R | E -> E


module Board = struct
  type board = 
      {
	mutable whos_turn:case;
	mutable moves:(int*int) list;
	mutable colones:int array;
	mutable board:case array array;
	mutable score_r:score;
	mutable score_y:score;
      }
  let board =  {whos_turn = R; 
		moves = []; 
		colones = Array.make 7 0; 
		board = Array.create_matrix 7 6 E; 
		score_r =N 0;
		score_y =N 0; 
	       }
  let print() = 
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
  let turn () = board.whos_turn
  let init () = 
    board.whos_turn <- R; board.moves <- []; 
    board.colones <- Array.make 7 0;
    board.board <-  Array.create_matrix 7 6 E;
    board.score_y <- N 0;
    board.score_r <- N 0
  let move x =
    let y = board.colones.(x) in
    board.board.(x).(y) <- board.whos_turn;
    board.colones.(x) <- y+1;
    board.whos_turn <- !!(board.whos_turn); 
    board.moves <- (x, y)::board.moves
  let cancel ()= 
    let x, y = List.hd board.moves in
    board.colones.(x) <- board.colones.(x) -1;
    board.board.(x).(y) <- E;
    board.whos_turn <- !!(board.whos_turn); 
    board.moves <- List.tl board.moves

  let get_moves () = 
  let l = ref [] in
    for i = 0 to 6 do
      if board.colones.(i) < 6 then l := i::!l;
    done;
    !l
  let get_turn () = board.whos_turn
  let get_score c = match c with
    | R -> board.score_r
    | _ -> board.score_y

  let edit_score c n = match c with
    | R -> board.score_r <- n
    | _ ->  board.score_y <- n
    
  let in_board (x, y) = 
    x >= 0 && y >= 0 && x < 7 && y < 6
  let case x y = board.board.(x).(y)
  let last_move () = List.hd (board.moves)
  let history_moves () = board.moves
end
;;
