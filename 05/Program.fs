open System.IO
open System
open System.Text.RegularExpressions

let inputText = File.ReadAllText("input.txt")

let [stackText; moveText;] = inputText.Split("\r\n\r\n") |> List.ofArray


type Stack<'a> =
    | Empty
    | Value of 'a * Stack<'a>

module Stack =
    let isEmpty = function
        | Empty   -> true
        | Value _ -> false
    let empty = Empty
    let push x stack = Value (x, stack) 
    let pop (Value (x, stack)) = x, stack
    let construct l =
        List.foldBack (fun x currentStack -> push x currentStack) l Stack.Empty

let stackCount = int stackText[stackText.Length - 2] - int '0'
let lineLength = stackCount * 4 + 1 //include \r\n char but no space at the end
let stackHeight = ((stackText.Length + 2 (* Missing /r/n at the end *) ) / lineLength - 1)

let coordsToIndex stackNo line lineLength =
    (line - 1) * lineLength + (stackNo - 1) * 4 + 1

let stackArray = [1..stackCount] |>
                    List.map(fun stackNumber -> [1..stackHeight] |>
                                                List.map(fun lineNumber -> stackText[coordsToIndex stackNumber lineNumber lineLength]) |>
                                                List.filter(fun x -> x <> ' ')) |>
                    List.map(fun l -> Stack.construct l) |>
                    Array.ofList


 
let pattern = @"move (\d+) from (\d+) to (\d+)"
let regex = Regex pattern

let extractMatches (m: Match) =
    m.Groups |> Seq.skip 1 |> Seq.map(fun g -> int g.Value)

let moves = moveText.Split("\r\n") |>
                Array.map(fun m -> m |> regex.Match |> extractMatches |> List.ofSeq) |>
                Array.map(fun [count; src; dest] -> (count, src, dest))


type moveSignature = Stack<char> array -> int -> int -> Stack<char> array
let doMove:moveSignature = fun stacks src dest ->
    let (valueToMove, newSrc) = Stack.pop stacks[src]
    let newDest = Stack.push valueToMove stacks[dest]
    stacks |> Array.mapi(fun i stack -> match i with
                                        | x when x = src -> newSrc
                                        | x when x = dest -> newDest
                                        | _ -> stack)

let stackAfterMoves = moves |> 
                        Array.fold (fun stacks (count, src, dest) -> [1..count] |> 
                                                                        List.fold (fun tmpStacks i -> doMove tmpStacks (src-1) (dest-1)) stacks
                        ) stackArray


let stackTops = stackAfterMoves |> Array.map(fun s -> s |> Stack.pop |> fst) |> System.String

printfn "%s" stackTops
    