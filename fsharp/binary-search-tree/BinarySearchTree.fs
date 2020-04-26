module BinarySearchTree

type Node = {
    data: int;
    leftNode: Node option;
    rightNode: Node option;
}

let left node  = node.leftNode

let right node = node.rightNode

let data node = node.data

let rec insertNode value node = 
    match node with
    | None -> Some {data = value; leftNode = None; rightNode = None}
    | Some n ->
        match n.data with 
        | x when value <= x -> 
            Some {n with leftNode = (insertNode value n.leftNode)}
        | x -> 
            Some {n with rightNode = (insertNode value n.rightNode)}
        

let create items = 
    let firstNum = List.head items
    let root = {data = firstNum; leftNode = None; rightNode = None}
    items 
    |> List.skip 1
    |> List.iter (fun x -> insertNode x (Some(root: Node)) |> ignore) 
    root

let rec sort node = seq {
    match node with
        | Some n ->
            yield! sort n.leftNode
            yield n.data
            yield! sort n.rightNode
        | None -> ()
}

let sortedData (node: Node) = 
    sort (Some(node)) 
    |> Seq.toList


let test = Some {data = 1; leftNode = None; rightNode = None}
let test2 = Some {data = 2; leftNode = test; rightNode = None}

let test3 = {test with leftNode = test2}