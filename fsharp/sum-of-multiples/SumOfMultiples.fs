module SumOfMultiples

let multipleOfNumbers (numbers: int list) (number: int): bool = 
    let moduloNums = List.map(fun x -> number % x) numbers
    List.min moduloNums = 0

let sum (numbers: int list) (upperBound: int): int = 
    let numbers = List.filter(fun x -> x <> 0) numbers
    if numbers.IsEmpty then 
        0
    else
        [1..upperBound-1]
        |> List.filter(fun (x: int) -> multipleOfNumbers numbers x)
        |> List.sum