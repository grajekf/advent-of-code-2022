open System.IO
open System

let inputLines = File.ReadAllLines("input.txt")

let calcPriority x = 
    match x with
    |a when x >= 'a' -> int x - int 'a' + 1 
    |a -> int x - int 'A' + 27

let commonElements = inputLines |> 
                        Array.map(fun l -> (l[.. l.Length / 2 - 1], l[l.Length / 2 ..])) |> 
                        Array.map(fun (first, second) -> (Set.ofSeq first, Set.ofSeq second)) |>
                        Array.map(fun (first, second) -> Set.intersect first second |> Seq.head) |>
                        Array.map(calcPriority)

Console.WriteLine(Array.sum commonElements)


let groupedLines = inputLines |> Array.chunkBySize 3

let commonElementsb = groupedLines |>
                        Array.map(fun group -> group |> 
                                                Array.map(Set.ofSeq) |> 
                                                Seq.ofArray |> 
                                                Set.intersectMany |>
                                                Seq.head
                        ) |>
                        Array.map(calcPriority)

Console.WriteLine(Array.sum commonElementsb)