module Raindrops

let numWord = [
    (3, "Pling");
    (5, "Plang");
    (7, "Plong")
]

let convert (number: int) = 
    numWord 
    |> List.map (fun (num, word) -> if number % num = 0 then word else "")
    |> String.concat ""
    |> fun resultStr ->
        match resultStr with
        | "" -> string number
        | _ -> resultStr
    