open System.IO

let inputLines = File.ReadAllLines("input.txt")
let assignments = inputLines |> 
                    Array.map(fun x -> x.Split(',') |> 
                                        Array.map(fun a -> a.Split('-')) |> 
                                        Array.map(fun l -> (int l[0], int l[1]))) |>
                    Array.map(fun x -> (x[0], x[1]))

let isAnyContained ((firstFrom, firstTo), (secondFrom, secondTo)) = 
    (firstFrom >= secondFrom && firstTo <= secondTo) || (secondFrom >= firstFrom && secondTo <= firstTo)


let containedCount = assignments |> Array.filter(isAnyContained) |> Array.length

printfn "%d" containedCount


let overlaps ((firstFrom, firstTo), (secondFrom, secondTo)) = 
    (firstFrom <= secondTo) && (firstTo >= secondFrom)

let overlapCount = assignments |> Array.filter(overlaps) |> Array.length

printfn "%d" overlapCount