open System.IO

let inputLines = File.ReadAllLines("input.txt")

type Move =
    |Rock 
    |Paper 
    |Scissors 

let moveMap = [ 'A', Move.Rock; 'B', Move.Paper; 'C', Move.Scissors; 'X', Move.Rock; 'Y', Move.Paper; 'Z', Move.Scissors ] |> dict

let pointsMap = [ Move.Rock, 1; Move.Paper, 2; Move.Scissors, 3 ] |> dict

let calcPoints [moveOne; moveTwo;] = 
    pointsMap[moveTwo] + 
    match moveOne,moveTwo with
        | Move.Rock, Move.Paper -> 6
        | Move.Paper, Move.Scissors -> 6
        | Move.Scissors, Move.Rock -> 6
        | x,y when x = y -> 3
        | _ -> 0

let scores = (
    inputLines 
    |> Array.map(
        fun x -> x.Split(" ") |> Array.map(fun m -> moveMap[m[0]]) |> List.ofArray |> calcPoints
    )
)

printfn "%d" (
    scores |> Array.sum
)

type Outcome = 
    | Loss = 0
    | Draw = 3
    | Win = 6

let outcomeMap = ['X', Outcome.Loss; 'Y', Outcome.Draw; 'Z', Outcome.Win ] |> dict

let getMoveForOutcome move outcome =
    match move,outcome with
    | x, Outcome.Draw -> x
    | Move.Rock, Outcome.Loss -> Move.Scissors
    | Move.Rock, Outcome.Win -> Move.Paper
    | Move.Scissors, Outcome.Loss -> Move.Paper
    | Move.Scissors, Outcome.Win -> Move.Rock
    | Move.Paper, Outcome.Loss -> Move.Rock
    | Move.Paper, Outcome.Win -> Move.Scissors

type CalcPoints2Type = Move -> Outcome -> int
let calcPoints2:CalcPoints2Type = fun move outcome ->
    (outcome |> int) + pointsMap[getMoveForOutcome move outcome]

let scoresb = (
    inputLines 
    |> Array.map(
        fun x -> x.Split(" ") |> fun m -> (moveMap[m[0][0]], outcomeMap[m[1][0]]) |> fun (move, outcome) -> calcPoints2 move outcome
    )
)

printfn "%d" (
    scoresb |> Array.sum
)