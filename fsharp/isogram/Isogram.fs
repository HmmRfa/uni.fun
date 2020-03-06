module Isogram

let isIsogram (str:string) : bool = 
    let formattedStr = String.map (System.Char.ToUpper) (str.Replace(" ", "").Replace("-", ""))
    Seq.distinct formattedStr
    |> Seq.toArray
    |> System.String = formattedStr
