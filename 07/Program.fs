open System.IO

let inputLines = File.ReadAllLines("input.txt") |> List.ofArray |> List.skip 1

type FileSystemEntry = 
    | File of size : int * path : string
    | Directory of path : string


type Command = 
    | ChangeDirectory of dir: string
    | List


let root = FileSystemEntry.Directory("/")

let goUp (dir: string) =
    dir.Substring(0, dir.LastIndexOf("/"))

let rec allDirs (dir: string) = 
    seq {
        yield dir;
        if dir <> "" then yield! allDirs(goUp dir) else yield! Seq.empty
    }

type ProcessCommandSignature = (string list) -> string -> (FileSystemEntry seq)
let rec processCommand: ProcessCommandSignature = fun inputLines currentDir ->
    if inputLines.IsEmpty then
        Seq.empty
    else
        let command = match inputLines.Head.Split(" ") |> List.ofArray with
                        | ["$"; "cd"; dir] -> Command.ChangeDirectory(dir)
                        | ["$"; "ls"] -> Command.List
        seq {
            match command with
            | Command.List -> 
                let listResults = inputLines.Tail |> List.takeWhile(fun l -> not(l.StartsWith "$"))
                yield! (listResults |> List.map(fun l -> match l.Split(" ") |> List.ofArray with
                                                                | ["dir"; dirName] -> FileSystemEntry.Directory(currentDir + "/" + dirName)
                                                                | [size; fileName] -> FileSystemEntry.File(int size, currentDir + "/" + fileName)));
                yield! (processCommand (inputLines.Tail |> List.skipWhile(fun l -> not(l.StartsWith "$"))) currentDir)
            | Command.ChangeDirectory(dir) when dir <> ".." ->
                yield! (processCommand inputLines.Tail (currentDir + "/" + dir))
            | _ -> yield! (processCommand inputLines.Tail (currentDir |> goUp))
        }


let allFiles = processCommand inputLines ""

let files = allFiles |> Seq.choose (fun f -> match f with | FileSystemEntry.File(size, path) -> Some (size, path) | FileSystemEntry.Directory(_) -> None )
let directories = allFiles |> Seq.choose (fun f -> match f with | FileSystemEntry.File(_, _) -> None | FileSystemEntry.Directory(path) -> Some path )

let dirsWithSize = files 
                        |> Seq.collect(fun (size, path) -> allDirs (goUp path) |> Seq.map (fun d -> (d, size))) 
                        |> Seq.groupBy(fun (d, size) -> d)
                        |> Seq.map(fun (d, sizes) -> d, sizes |> Seq.sumBy (fun (d, size) -> size))


let smallDirs = dirsWithSize |> Seq.filter(fun (dir, size) -> size <= 100000)
let smallDirsSizeSum = smallDirs |> Seq.sumBy(fun (dir, size) -> size)

printfn "%d" smallDirsSizeSum

let allDiskSpace = 70000000
let neededDiskSpace = 30000000
let (_, currentlyUsedDiskSpace) = dirsWithSize |> Seq.find(fun (dir, size) -> dir = "")
let currentlyFreeDiskSpace = allDiskSpace - currentlyUsedDiskSpace
let diskSpaceToFree = neededDiskSpace - currentlyFreeDiskSpace

printfn "%d" currentlyUsedDiskSpace
printfn "%d" currentlyFreeDiskSpace
printfn "%d" diskSpaceToFree

let dirToDelete = dirsWithSize
                    |> Seq.filter(fun (dir, size) -> size >= diskSpaceToFree)
                    |> Seq.sortBy(fun (dir, size) -> size)
                    |> Seq.head

printfn "%A" dirToDelete