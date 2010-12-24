open Extendn;; 
open Board;; 


let spec_count  (x,y) (i,j) piece = 
  let rec count (x, y) = 
    if not (Board.in_board (x, y)) then 0
    else
      match Board.case x y with
	| p when p = piece -> 1 + count (x+i, y+j)
	| _ -> 0
  in count (x+i, y+j)
;;

let count (x, y) (i, j) = 
  let piece = Board.case x y in 
  spec_count (x,y) (i,j) piece
;;

let who_won ()=
  try 
    let x, y = Board.last_move() in
      if count (x, y) (1,0) + count (x, y) (-1,0) >= 3 
	|| count (x, y) (0,1) + count (x, y) (0,-1) >= 3 
	|| count (x, y) (1,1) + count (x, y) (-1,-1) >= 3 
	|| count (x, y) (-1,1) + count (x, y) (1,-1) >= 3 
      then Board.case x y
      else E
  with _ -> E
;;

let eval ()= 
  if  Board.history_moves () = [] then N 0
  else
    let x, y = Board.last_move() in
    let color = Board.case x y in
    if who_won () = color then PInf
    else
      let prec_score = Board.get_score color in
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
      let final_score = N (tbl.(y).(x)) ++ prec_score in
      Board.edit_score color final_score;
      final_score
      
;;
