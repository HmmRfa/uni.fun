module Accumulate

let reverse list = 
    let rec _reverse list acc =
        match list with
        | [] -> acc
        | x::xs -> _reverse xs (x::acc)
    _reverse list []    

let accumulate (func: 'a -> 'b) (input: 'a list): 'b list = 
    let rec calc f list acc =
        match list with 
        | [] -> acc |> reverse
        | x::xs -> calc f xs ((f x)::acc)
    calc func input []    

