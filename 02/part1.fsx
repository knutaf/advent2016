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

type Position = { x : int; y : int };;
type State = { history : Position list; pos : Position };;

let MAX_X = 2;;
let MAX_Y = 2;;

let getKeyFromCoordinates pos =
    match (pos.y, pos.x) with
    | (0, 0) -> 1
    | (0, 1) -> 2
    | (0, 2) -> 3
    | (1, 0) -> 4
    | (1, 1) -> 5
    | (1, 2) -> 6
    | (2, 0) -> 7
    | (2, 1) -> 8
    | (2, 2) -> 9
    | _ -> raise (Ex (sprintf "invalid coords: %d, %d" pos.x pos.y))
;;

let makeMove pos moveChar =
    (*printfn "makeMove %A %c" pos moveChar;*)
    match moveChar with
    | 'U' -> if pos.y = 0 then pos else { x = pos.x; y = pos.y - 1 }
    | 'D' -> if pos.y = MAX_Y then pos else { x = pos.x; y = pos.y + 1 }
    | 'L' -> if pos.x = 0 then pos else { x = pos.x - 1; y = pos.y }
    | 'R' -> if pos.x = MAX_X then pos else { x = pos.x + 1; y = pos.y }
    | _ -> raise (Ex (sprintf "invalid move: %c" moveChar))
;;

let rec processLine state =
    let line = Console.ReadLine() in
    (*printfn "line: %s" line;*)
    if String.length line = 0 then
        state
    else
        let nextPos = Seq.fold makeMove state.pos line in
        (*printfn "nextPos = %A (%d)" nextPos (getKeyFromCoordinates nextPos);*)
        processLine { history = List.append state.history [state.pos]; pos = nextPos }
;;

let state = processLine { history = []; pos = { x = 1; y = 1 } } in
List.iter (fun elem -> printf "%d" (getKeyFromCoordinates elem)) (List.tail state.history);
printf "%d" (getKeyFromCoordinates state.pos);;
