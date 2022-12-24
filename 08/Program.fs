open System.IO
open System

let inputLines = File.ReadAllLines("input.txt")

let rowCount = inputLines.Length
let columnCount = inputLines[0].Length

let map = inputLines |> Array.map(fun l -> l |> Seq.map(fun c -> int c - int '0') |> Array.ofSeq)


let scanFromStart map =
    map |> Array.map(fun row -> row |> (Array.scan (fun cummax (x: int) -> Math.Max(cummax,x)) -1)
                                                            |> Array.rev 
                                                            |> Array.skip 1 
                                                            |> Array.rev)

let scanFromEnd map =
    map |> Array.map(fun row -> (row, -1) ||> (Array.scanBack (fun x cummax -> Math.Max(cummax, x))) 
                                                            |> Array.skip 1)

let mapFromLeft = map |> scanFromStart
let mapFromRight = map |> scanFromEnd

let mapFromTop = map |> Array.transpose |> scanFromStart |> Array.transpose
let mapFromBottom = map |> Array.transpose |> scanFromEnd |> Array.transpose

let flatten map =
    map |> Array.fold Array.append Array.empty<int>


let visibilityMap = Array.zip3 (flatten map)  (flatten mapFromLeft) (Array.zip3 (flatten mapFromRight)  (flatten mapFromTop) (flatten mapFromBottom))
                        |> Array.map (fun (m, l, (r, t, b)) -> m > l || m > r || m > t || m > b)

let visibleCount = visibilityMap |> Array.filter(fun x -> x) |> Array.length

printfn "%d" visibleCount


let rec allTails a = 
    if (a |> Array.length) = 1 then 
        [[(a |> Array.head)]]
    else
        [
            yield (a |> List.ofArray)
            yield! a |> Array.tail |> allTails
        ]


let viewDistance height view =
    let viewCount = view |> List.takeWhile (fun x -> x < height) |> List.length
    if viewCount < (view |> List.length) then
        viewCount + 1
    else
        viewCount


let lookingFromStart map =
    map |> Array.map (fun row -> row 
                                |> allTails 
                                |> List.map (fun l -> match l with
                                                        | [head] -> 0
                                                        | head :: tail -> viewDistance head tail)
                                |> List.toArray)

let lookingFromEnd map =
    map |> Array.map (fun row -> row 
                                |> Array.rev
                                |> allTails 
                                |> List.map (fun l -> match l with
                                                        | [head] -> 0
                                                        | head :: tail -> viewDistance head tail)
                                |> List.rev
                                |> List.toArray)

let lookingRightDistance = lookingFromStart map
let lookingLeftDistance = lookingFromEnd map

let lookingDownDistance = map |> Array.transpose |> lookingFromStart |> Array.transpose
let lookingUpDistance = map |> Array.transpose |> lookingFromEnd |> Array.transpose

let scenicScores = Array.zip3 (flatten lookingRightDistance)  (flatten lookingLeftDistance) (Array.zip (flatten lookingDownDistance)  (flatten lookingUpDistance))
                        |> Array.map (fun (r, l, (d, u)) -> r * l * d * u)

let maxScenicScore = scenicScores |> Array.max

printfn "%d" maxScenicScore

