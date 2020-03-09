module QueenAttack

let create (position: int * int) = 
    match position with
    | (x, y) when x < 0 || y < 0 -> false
    | (x, y) when x < 8 && y < 8 -> true
    | _ -> false 

let canAttack (queen1: int * int) (queen2: int * int) = 
    if fst queen1 = fst queen2 then
        true
    elif snd queen1 = snd queen2 then
        true 
    else
        abs (fst queen1 - fst queen2) = abs (snd queen1 - snd queen2)