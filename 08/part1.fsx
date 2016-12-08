open System;;
open System.Text.RegularExpressions;;

exception Ex of string;;

(*
let GRID_WIDTH = 50;;
let GRID_HEIGHT = 6;;
*)

let GRID_WIDTH = 7;;
let GRID_HEIGHT = 3;;

type Instruction =
    | Rect of int * int
    | RotateRow of int * int
    | RotateCol of int * int
;;

let rexmatch pat str =
    let m = Regex.Match(str, pat) in
    if m.Groups.Count = 0 then
        []
    else
        List.tail [ for g in m.Groups -> g.Value ]
;;

let parseInstruction line =
    match rexmatch @"^rect (\d+)x(\d+)$" line with
    | [a; b] -> Rect(Convert.ToInt32(a), Convert.ToInt32(b))
    | [] ->
        match rexmatch @"^rotate row y=(\d+) by (\d+)$" line with
        | [a; b] -> RotateRow(Convert.ToInt32(a), Convert.ToInt32(b))
        | [] ->
            match rexmatch @"^rotate column x=(\d+) by (\d+)$" line with
            | [a; b] -> RotateCol(Convert.ToInt32(a), Convert.ToInt32(b))
            | _ -> raise (Ex (sprintf "invalid directive! %s" line))
        | _ -> raise (Ex (sprintf "invalid directive! %s" line))
    | _ -> raise (Ex (sprintf "invalid directive! %s" line))
;;

let stringOfGrid =
    let collectColumns sofar =
        Array.fold (fun sofar c -> if c then sofar + "#" else sofar + ".") sofar
    in
    Array.fold (fun sofar row -> (collectColumns sofar row) + "\r\n") ""
;;

let applyInstructionToGrid inst grid =
    printfn "inst: %A" inst;
    printfn "%s" (stringOfGrid grid);
    grid
;;

let countLit grid =
    0
;;

let rec processLines grid =
    let line = Console.ReadLine() in
    if String.length line = 0 then
        grid
    else
        let inst = parseInstruction line in
        processLines (applyInstructionToGrid inst grid)
;;

let create2DArrayOfArrays rows cols initialValue =
    [|
        for r in 1 .. rows ->
        [|
            for c in 1 .. cols -> initialValue
        |]
    |]
;;

let finalGrid = processLines (create2DArrayOfArrays GRID_HEIGHT GRID_WIDTH false) in
printfn "num lit: %u" (countLit finalGrid)
;;
