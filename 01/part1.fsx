open System;;
open System.Text.RegularExpressions;;

exception Ex of string;;

let rexmatch pat str =
    let m = Regex.Match(str, pat) in
    if m.Groups.Count = 0 then
        []
    else
        List.tail [ for g in m.Groups -> g.Value ]
;;

type Direction =
    | North
    | South
    | East
    | West
;;

type TurnDirection =
    | Left
    | Right
;;

type Position = { x : int; y : int; dir : Direction};;
type Move = { turnDir : TurnDirection; spaces : int };;

let parseMove str =
    let parts = rexmatch "^(R|L)(\\d+)$" str in
    let (turnStr, spacesStr) =
        match parts with
        [] -> raise (Ex (sprintf "failed to match a direction! %s" str))
        | head :: tail :: [] -> (head, tail)
        | _ -> raise (Ex "invalid matches!")
    in
    let turnDir =
        match turnStr with
        | "L" -> Left
        | "R" -> Right
        | _ -> raise (Ex (sprintf "bad turn dir! %s" turnStr));
    in
    { turnDir = turnDir; spaces = Convert.ToInt32 spacesStr }
;;

let makeMove pos moveStr =
    let move = parseMove moveStr in
    let newDir =
        match pos.dir with
        | North -> if move.turnDir = Left then West else East
        | South -> if move.turnDir = Left then East else West
        | East -> if move.turnDir = Left then North else South
        | West -> if move.turnDir = Left then South else North
    in
    let (dx, dy) =
        match newDir with
        | North -> (0, -1)
        | South -> (0, 1)
        | East -> (1, 0)
        | West -> (-1, 0)
    in
    { x = pos.x + (dx * move.spaces); y = pos.y + (dy * move.spaces); dir = newDir }
;;

let moves = Console.ReadLine().Split([|", "|], StringSplitOptions.None) in
let finalPos = Array.fold makeMove { x = 0; y = 0; dir = North } moves in
printfn "x = %d, y = %d, dist=%d" finalPos.x finalPos.y ((abs finalPos.x) + (abs finalPos.y))
;;
