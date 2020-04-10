module Raindrops

let numWord = [
    (3, "Pling");
    (5, "Plang");
    (7, "Plong")
]

let convert (number: int) = 
    let wordList = List.map (fun (num, word) -> if number % num = 0 then word else "") numWord
    let result = String.concat "" wordList
    if result = "" then
        string number
    else   
        result    