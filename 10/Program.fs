open System.IO

type Operation =
    | Noop
    | Add of x: int

let inputLines = File.ReadAllLines("input.txt")
let operations = inputLines 
                |> Array.map (fun line -> line.Split(" ")) 
                |> Array.map (fun split -> match split with
                                            | [|"noop"|] -> Operation.Noop
                                            | [|"addx"; x|] -> Operation.Add(int x))

let doOperation register operation =
    match operation with
        | Operation.Noop -> [|register|]
        | Operation.Add(x) -> [|register; register + x|]

let allRegisterValues = operations 
                        |> Array.scan (fun register operation ->  doOperation (register |> Array.last) operation) [|1|]
                        |> Array.collect id


let signalStrength allRegisterValues index =
    ((index - 1) |> Array.get allRegisterValues) * index

let twentyStrength = signalStrength allRegisterValues 20
let sixtyStrength = signalStrength allRegisterValues 60
let hundredStrength = signalStrength allRegisterValues 100
let hundredFortyStrength = signalStrength allRegisterValues 140
let hundredEightyStrength = signalStrength allRegisterValues 180
let twoTwentyStrength = signalStrength allRegisterValues 220

printfn "%d" (twentyStrength + sixtyStrength + hundredStrength + hundredFortyStrength + hundredEightyStrength + twoTwentyStrength)
