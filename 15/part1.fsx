open System;;
open System.Text.RegularExpressions;;

exception Ex of string;;

type Disc = { num : int; numPositions : int; currentPosition : int };;

let rexmatch pat str =
    let m = Regex.Match(str, pat) in
    if m.Groups.Count = 0 then
        []
    else
        List.tail [ for g in m.Groups -> g.Value ]
;;

let parseDisc line =
    match rexmatch @"^Disc \#(\d+) has (\d+) positions; at time=0, it is at position (\d+)\.$" line with
    | [numStr; numPositionsStr; startingPositionStr] ->
        let numPositions = Convert.ToInt32(numPositionsStr) in
        { num = Convert.ToInt32(numStr); numPositions = numPositions; currentPosition = ((Convert.ToInt32(startingPositionStr)) + 1) % numPositions }
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

let advanceDiscs discs =
    List.map (fun disc -> { disc with currentPosition = ((disc.currentPosition + 1) % disc.numPositions) }) discs
;;

let rec doesFallThrough discs =
    match discs with
    | [] -> true
    | disc :: restOfDiscs ->
        if disc.currentPosition = 0 then
            doesFallThrough (advanceDiscs restOfDiscs)
        else
            false
;;

let findFirstGoodStartingTime discs =
    let rec helper discs startingTime =
        if doesFallThrough discs then
            startingTime
        else
            helper (advanceDiscs discs) (startingTime + 1)
    in
    helper discs 0
;;

let discs = loadDiscs [] in
let goodStartingTime = findFirstGoodStartingTime discs in
printfn "ball fell through at time=%d" goodStartingTime
;;
