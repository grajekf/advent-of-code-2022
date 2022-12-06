open System.IO

let inputText = File.ReadAllText("input.txt")

let startOfPacket = inputText |> List.ofSeq |> List.windowed 4 |> List.findIndex (fun window -> (window |> List.distinct |> List.length) = 4)

printfn "%d" (startOfPacket + 4)

let startOfMessage = inputText |> List.ofSeq |> List.windowed 14 |> List.findIndex (fun window -> (window |> List.distinct |> List.length) = 14)

printfn "%d" (startOfMessage + 14)
