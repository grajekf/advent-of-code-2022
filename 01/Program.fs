open System.IO
open System

let inputText = File.ReadAllText("inputa.txt")

let allSums = (
    inputText.Split("\r\n\r\n") 
    |> Array.toList 
    |> List.map(fun x -> (
        x.Split("\r\n", StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries) 
            |> Array.toList 
            |> List.map(int) 
            |> List.sum)
        ) 
)

//1a
printfn "%s" (
    allSums
    |> List.max
    |> string
)

//1b
printfn "%s" (
    allSums
    |> List.sortDescending
    |> Seq.take 3
    |> Seq.sum
    |> string
)