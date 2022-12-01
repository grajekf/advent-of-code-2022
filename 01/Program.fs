open System.IO
open System

let inputText = File.ReadAllText("inputa.txt")

printfn "%s" (
    inputText.Split("\r\n\r\n") 
        |> Array.toList 
        |> List.map(fun x -> (
            x.Split("\r\n", StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries) 
                |> Array.toList 
                |> List.map(int) 
                |> List.sum)
            ) 
        |> List.max
        |> string
)