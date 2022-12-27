open System.IO
open System
open System.Text.RegularExpressions

let inputText = File.ReadAllText("input.txt")
let monkeyTexts = inputText.Split("\r\n\r\n") 
                |> Array.map (fun x -> x.Split("\r\n") |> Array.map(fun s -> s.Trim())) 
                |> Array.map (fun [|monkeyName;startingItems;operation;test;ifTrue;ifFalse|] -> (monkeyName,startingItems,operation,test,ifTrue,ifFalse))


let extractIntMatches (m: Match) =
    m.Groups |> Seq.skip 1 |> Seq.map(fun g -> int g.Value)


let monkeyNamePattern = @"Monkey (\d+):"
let monkeyNameRegex = Regex monkeyNamePattern

let operationPattern = @"new = ([^ ]*) ([\*\+]) ([^ ]*)"
let operationRegex = Regex operationPattern


let monkeys = monkeyTexts
            |> Array.map(fun (monkeyName,startingItemsString,operationString,testString,ifTrueString,ifFalseString) -> 
                                let monkeyNumber = monkeyNameRegex.Match monkeyName |> extractIntMatches |> Seq.head
                                let startingItems = startingItemsString.Substring(startingItemsString.IndexOf(":") + 2).Split(", ")
                                                    |> Array.map int
                                                    |> List.ofArray
                                let [a;operation;b] = (operationRegex.Match operationString).Groups
                                                        |> Seq.skip 1 
                                                        |> Seq.map (fun m -> m.Value)
                                                        |> List.ofSeq

                                let divisibleBy = testString.Substring(testString.LastIndexOf(' ') + 1) |> int
                                let ifTrue = ifTrueString.Substring(ifTrueString.LastIndexOf(' ') + 1) |> int
                                let ifFalse = ifFalseString.Substring(ifFalseString.LastIndexOf(' ') + 1) |> int
                                (monkeyNumber, startingItems, (a, operation, b), divisibleBy, ifTrue, ifFalse)
                        )

printfn "%A" monkeys

let getValue text currentWorryLevel =
    match text with
    | "old" -> currentWorryLevel
    | x -> int x

let doOperation (a, operation, b) currentWorryLevel =
    match operation with
    | "+" -> (getValue a currentWorryLevel) + (getValue b currentWorryLevel)
    | "*" -> (getValue a currentWorryLevel) * (getValue b currentWorryLevel)

let getItemsFromChanges itemChanges index =
    itemChanges 
        |> List.filter (fun (newMonkey, _) -> index = newMonkey)
        |> List.map snd

let simulateTurn monkeys index =
    let (monkeyNumber, items, (a, operation, b), divisibleBy, ifTrue, ifFalse) = Array.get monkeys index
    
    let itemChanges = items
                    |> List.map (fun item ->
                                    let newWorryLevel = doOperation (a, operation, b) item |> float
                                    let dividedWorry = newWorryLevel / 3.0 |> floor |> int
                                    ((if dividedWorry % divisibleBy = 0 then ifTrue else ifFalse), dividedWorry)
                                )

    let inspections = (index, items.Length)
                                                                          
    (
        monkeys |> Array.map (fun (monkeyNumber, items, (a, operation, b), divisibleBy, ifTrue, ifFalse) ->
                                (
                                    monkeyNumber, 
                                    (if monkeyNumber = index then [] else items @ (getItemsFromChanges itemChanges monkeyNumber)), 
                                    (a, operation, b), 
                                    divisibleBy, 
                                    ifTrue, 
                                    ifFalse
                                )
                            ),
        inspections
    )

let simulateRound monkeys =
    monkeys |> Array.fold (fun (monkeys, inspections) (monkeyNumber, _, _, _, _, _) ->
                                let (newMonkeys, newInspections) = simulateTurn monkeys monkeyNumber
                                (newMonkeys, newInspections :: inspections)) (monkeys, [])

let (monkeysAfterSimluations, inspections) = [1..20] |> List.fold (fun (monkeys, inspections) _ ->
                                let (newMonkeys, newInspections) = simulateRound monkeys
                                (newMonkeys, inspections @ newInspections)) (monkeys, [])
let groupedInspections = inspections 
                        |> List.groupBy (fun (monkeyNumber, _) -> monkeyNumber)
                        |> List.map (fun (monkeyNumber, monkeyInspections) -> (monkeyNumber, monkeyInspections 
                                                                                                |> List.sumBy (fun (_, count) -> count)))

printfn "%A" monkeysAfterSimluations
printfn "%A" groupedInspections
