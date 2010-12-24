(* open Types;; *)
(* open Extendn;; *)

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

let eval board = 
  if board.moves = [] then N 0
  else
    let x, y = List.hd (board.moves) in
    let color = board.board.(x).(y) in
    if who_won board = color then PInf
    else
      let tbl =
        [|
        [| 0;2;3;4;3;2;0 |];
        [| 0;2;4;5;4;2;0 |];
        [| 0;2;4;5;4;2;0 |];
        [| 0;2;3;5;3;2;0 |];
        [| 0;2;2;2;2;2;0 |];
        [| 0;1;1;1;1;1;0 |];
	|]
      in
      N (tbl.(y).(x))
;;
