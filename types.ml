type case = R | Y | E;;
type board = 
{
whos_turn:case;
moves:(int*int) list;
colones:int array;
board:case array array;
};;

let (!!) = function R -> Y | Y -> R | E -> E;;