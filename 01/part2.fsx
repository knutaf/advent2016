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

type Position = { x : int; y : int; dir : Direction; fin : bool };;
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

let isPerpendicularTo dir1 dir2 =
    let check dir1 dir2 =
        ((dir1 = North || dir1 = South) && (dir2 = East || dir2 = West))
    in
    (check dir1 dir2) || (check dir2 dir1)
;;

let lineLength pos1 pos2 =
    (abs (pos2.x - pos1.x)) + (abs (pos2.y - pos1.y))
;;

let getLineIntersection pos1 pos2 pos3 pos4 =
    if isPerpendicularTo pos2.dir pos4.dir then
        let minPos = (List.min [pos1.x; pos2.x; pos3.x; pos4.x],
                      List.min [pos1.y; pos2.y; pos3.y; pos4.y]) in
        let maxPos = (List.max [pos1.x; pos2.x; pos3.x; pos4.x],
                      List.max [pos1.y; pos2.y; pos3.y; pos4.y]) in
        let maxArea = ((fst maxPos) - (fst minPos)) * ((snd maxPos) - (snd minPos)) in
        let lineLength1 = lineLength pos1 pos2 in
        let lineLength2 = lineLength pos3 pos4 in
        let linesArea = lineLength1 * lineLength2 in
        if maxArea = linesArea then
            if pos1.y = pos2.y then
                Some (pos3.x, pos1.y)
            else
                Some (pos1.x, pos3.y)
        else
            None
    else
        None
;;

let makeMove poses moveStr =
    if (List.head poses).fin then
        poses
    else
        let move = parseMove moveStr in
        let pos = List.head poses in
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
        let maybeNextPos = { x = pos.x + (dx * move.spaces); y = pos.y + (dy * move.spaces); dir = newDir; fin = false } in
        let rec findPreviousTraversal previousPoses =
            match previousPoses with
            | [] -> None
            | _ :: [] -> None
            | pos2 :: pos1 :: rest ->
                match getLineIntersection pos1 pos2 pos maybeNextPos with
                | None -> findPreviousTraversal (pos1 :: rest)
                | Some (intersectX, intersectY) -> Some { x = intersectX; y = intersectY; dir = maybeNextPos.dir; fin = true }
        in
        match findPreviousTraversal (List.tail poses) with
        | None -> maybeNextPos :: poses
        | Some intersectPos -> intersectPos :: poses
;;

let moves = Console.ReadLine().Split([|", "|], StringSplitOptions.None) in
let poses = Array.fold makeMove [{ x = 0; y = 0; dir = North; fin = false }] moves in
(*List.iter (fun elem -> printfn "move: %A" elem) poses;*)
let finalPos = List.head poses in
printfn "final: %A, dist=%d" finalPos ((abs finalPos.x) + (abs finalPos.y))
;;
