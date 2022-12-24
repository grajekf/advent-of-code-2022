open System.IO

let inputLines = File.ReadAllLines("input.txt")
let moves = inputLines |> Array.map (fun line -> line.Split(" ")) |> Array.map (fun [|first;second|] -> (first[0], int second))

let isAdjecant (ax: int, ay: int) (bx: int, by: int) = 
    abs (ax - bx) <= 1 && abs (ay - by) <= 1


let rec moveUntilAdjecant (headX, headY) (tailX, tailY) =
    if isAdjecant (headX, headY) (tailX, tailY) then
        (tailX, tailY)
    else
        moveUntilAdjecant (headX, headY) (tailX + sign (headX - tailX), tailY + sign (headY - tailY))

let doMove (headX, headY) (tailX, tailY) direction count =
    let (newHeadX, newHeadY) = match direction with
                                | 'L' -> (headX - count, headY)
                                | 'R' -> (headX + count, headY)
                                | 'U' -> (headX, headY + count)
                                | 'D' -> (headX, headY - count)
    ((newHeadX, newHeadY), moveUntilAdjecant (newHeadX, newHeadY) (tailX, tailY))


let splitMoves = moves |> Array.collect (fun (direction, count) -> [|1..count|] |> Array.map(fun _ -> (direction, 1)))


let allPositions = splitMoves |> Array.scan (fun ((headX, headY), (tailX, tailY)) (direction, count) -> doMove (headX, headY) (tailX, tailY) direction count) ((0,0),(0,0))

let allTailPositions = allPositions |> Array.map (fun (_, (tailX, tailY)) -> (tailX, tailY)) |> Array.distinct
let tailPositionCount = allTailPositions |> Array.length
printfn "%d" tailPositionCount


let doNineTailMove (headX, headY) tailArray direction count =
    let (newHeadX, newHeadY) = match direction with
                                | 'L' -> (headX - count, headY)
                                | 'R' -> (headX + count, headY)
                                | 'U' -> (headX, headY + count)
                                | 'D' -> (headX, headY - count)

    let newTailPositions = tailArray |> Array.scan (fun (prevX, prevY) (tailX, tailY) -> moveUntilAdjecant (prevX, prevY) (tailX, tailY)) (newHeadX, newHeadY)
    ((newHeadX, newHeadY), newTailPositions |> Array.skip 1)


let allNineTailPositions = splitMoves |> Array.scan (fun ((headX, headY), tailArray) (direction, count) -> doNineTailMove (headX, headY) tailArray direction count) ((0,0), Array.create 9 (0,0))
let allNinthTailPositions = allNineTailPositions |> Array.map (fun (_, tailArray) -> tailArray |> Array.last) |> Array.distinct
let ninthTailPositionCount = allNinthTailPositions |> Array.length
printfn "%d" ninthTailPositionCount