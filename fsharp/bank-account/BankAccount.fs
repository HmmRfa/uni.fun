module BankAccount

type BankAccount = {
    mutable balance: decimal;
}

type Status = 
    | OpenAcc of BankAccount
    | ClosedAcc

let mkBankAccount() = ClosedAcc

let openAccount account = 
    OpenAcc {balance = 0.0m}

let closeAccount account = 
    match account with
    | OpenAcc status -> ClosedAcc
    | ClosedAcc _ -> failwith "Account closed already."


let getBalance account = 
    match account with
    | OpenAcc status -> Some status.balance
    | ClosedAcc _ -> None

let updateBalance change account = 
    match account with
    | OpenAcc status -> 
        lock (status) (fun _ -> 
            status.balance <- status.balance + change
            OpenAcc status)
    | ClosedAcc _ -> failwith "Account closed."


    