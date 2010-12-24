(* Extension de N : ajout de +oo et -oo pour le gain et la défaite. *)

type score = PInf | N of int | MInf
let (--) = function
    | PInf -> MInf
    | MInf -> PInf
    | N n -> N (-n)
let (++) a b = match a, b with
  | (N a, N b) -> N (a+b)
  | (PInf, N _) -> PInf
  | (MInf, N _) -> MInf
  | (N _, PInf) -> PInf
  | (N _, MInf) -> MInf
  | (MInf, MInf) -> MInf
  | (PInf, PInf) -> PInf
  | _ -> failwith "-oo + +oo is a non-sens"
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

