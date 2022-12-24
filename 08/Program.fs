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



