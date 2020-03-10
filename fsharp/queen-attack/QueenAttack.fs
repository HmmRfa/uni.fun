module QueenAttack

let create (x: int, y: int) = 
    if x < 0 || y < 0 then
        false
    else
        x < 8 && y < 8  

let canAttack (queen1: int * int) (queen2: int * int) = 
    if fst queen1 = fst queen2 then
        true
    elif snd queen1 = snd queen2 then
        true 
    else
        abs (fst queen1 - fst queen2) = abs (snd queen1 - snd queen2)