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

let KEYPAD =
    [|
        [| None;     None;     Some '1'; None;     None;     |];
        [| None;     Some '2'; Some '3'; Some '4'; None;     |];
        [| Some '5'; Some '6'; Some '7'; Some '8'; Some '9'; |];
        [| None;     Some 'A'; Some 'B'; Some 'C'; None;     |];
        [| None;     None;     Some 'D'; None;     None;     |];
    |]
;;

let STARTING_POS = { x = 0; y = 2 };;

let getKeyFromCoordinates pos =
    if pos.y >= 0 && pos.y < Array.length KEYPAD &&
       pos.x >= 0 && pos.x < Array.length KEYPAD.[pos.y] then
        KEYPAD.[pos.y].[pos.x]
    else
        None
;;

let getKeyCharFromCoordinates pos =
    match getKeyFromCoordinates pos with
    | None -> '?'
    | Some k -> k
;;

let makeMove pos moveChar =
    (*printfn "makeMove %A %c" pos moveChar;*)
    let nextPos =
        match moveChar with
        | 'U' -> { x = pos.x; y = pos.y - 1 }
        | 'D' -> { x = pos.x; y = pos.y + 1 }
        | 'L' -> { x = pos.x - 1; y = pos.y }
        | 'R' -> { x = pos.x + 1; y = pos.y }
        | _ -> raise (Ex (sprintf "invalid move: %c" moveChar))
    in
    let key = getKeyFromCoordinates nextPos in
        match key with
        | None -> pos
        | Some k -> nextPos
    ;
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

let state = processLine { history = []; pos = STARTING_POS } in
List.iter (fun elem -> printf "%c" (getKeyCharFromCoordinates elem)) (List.tail state.history);
printf "%c" (getKeyCharFromCoordinates state.pos);;
