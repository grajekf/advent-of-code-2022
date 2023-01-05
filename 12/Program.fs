open System.IO
open System

let inputLines = File.ReadAllLines("input.txt")

let rowCount = inputLines.Length
let columnCount = inputLines[0].Length

let map = inputLines |> Array.map(fun l -> l |> Seq.map(fun c -> int c - int 'a') |> Array.ofSeq)
let mapFlat = map |> Array.collect id

//printfn "%A" map

let charToValue char =
    int char - int 'a'

let startValue = charToValue 'S'
let targetValue = charToValue 'E'

let indexToPosition index =
    (index % columnCount, index / columnCount)

let positionToIndex (x, y) =
    y * columnCount + x

let startPosition = mapFlat |> Array.findIndex (fun x -> x = startValue) |> indexToPosition
let endPosition = mapFlat |> Array.findIndex (fun x -> x = targetValue) |> indexToPosition


//printfn "%A" (positionToIndex startPosition)
//printfn "%A" (positionToIndex endPosition)

let normalizedMap = map |> Array.map (fun row -> row |> Array.map (fun x -> match x with 
                                                                            | a when a = startValue -> charToValue 'a' 
                                                                            | a when a = targetValue -> charToValue 'z'
                                                                            | a -> a))

//printfn "%A" normalizedMap

let generateNeighbours (x, y) =
    [
        if x < columnCount - 1 then yield (x + 1, y)
        if y < rowCount - 1 then yield (x, y + 1)
        if x > 0 then yield (x - 1, y)
        if y > 0 then yield (x, y - 1)
    ]

let getElevation map (x, y) =
    Array.get (Array.get map y) x

let rec findWay map target queue visited =
    match queue with
        | [] -> []
        | (position, trace) :: tail ->
            if position = target then
                (List.rev (position :: trace))
            else
                let elevation = getElevation map position
                let newVisited = visited |> Set.add position
                let possibleNeighbours = generateNeighbours position 
                                        |> List.filter (fun newPosition -> (getElevation map newPosition) <= (elevation + 1))
                                        |> List.filter (fun newPosition -> newVisited.Contains(newPosition) |> not)
                let newAllVisited = newVisited |> Set.union (possibleNeighbours |> Set.ofList)
                findWay map target (tail @ (possibleNeighbours |> List.map (fun newPosition -> (newPosition, newPosition :: trace)))) newAllVisited

let way = findWay normalizedMap endPosition [(startPosition, [startPosition])] Set.empty

printfn "%A" way
printfn "%A" (way.Length - 2) //Without start and end


let allPossibleStartingPositions = mapFlat 
                                |> Array.indexed 
                                |> Array.filter  (fun (_, x) -> x = charToValue 'a') 
                                |>  Array.map fst 
                                |> Array.map indexToPosition

let allWayLengths = allPossibleStartingPositions 
                    |> Array.map (fun start -> findWay normalizedMap endPosition [(start, [start])] Set.empty)
                    |> Array.filter (fun x -> x.Length > 0)
                    |> Array.map (fun x -> x.Length)

let shortestLength = allWayLengths |> Array.min
printfn "%A" (shortestLength - 2) //Without start and end
