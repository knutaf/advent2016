open System;;
open System.Text.RegularExpressions;;

exception Ex of string;;

type Disc = { num : int; numPositions : int; startingPosition : int };;

let rexmatch pat str =
    let m = Regex.Match(str, pat) in
    if m.Groups.Count = 0 then
        []
    else
        List.tail [ for g in m.Groups -> g.Value ]
;;

let parseDisc line =
    match rexmatch @"^Disc \#(\d+) has (\d+) positions; at time=0, it is at position (\d+)\.$" line with
    | [numStr; numPositionsStr; startingPositionStr] -> { num = Convert.ToInt32(numStr); numPositions = Convert.ToInt32(numPositionsStr); startingPosition = Convert.ToInt32(startingPositionStr) }
    | _ -> raise (Ex (sprintf "invalid disc! %s" line))
;;

let rec loadDiscs discsSoFar =
    let line = Console.ReadLine() in
    if String.length line = 0 then
        discsSoFar
    else
        let disc = parseDisc line in
        loadDiscs (discsSoFar @ [disc])
;;

let discs = loadDiscs [] in
printfn "%A" discs
;;
