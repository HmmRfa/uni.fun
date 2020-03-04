module TwoFer

let twoFer (input: string option): string = 
    if input.IsSome then"One for " + input.Value + ", one for me."
    else "One for you, one for me."