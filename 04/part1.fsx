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

type room = { name : string; sector : int; checksum : string };;

let parseRoom line =
    let parts = rexmatch "(.*)-(\d+)\\[(\w+)\\]$" line in
    match parts with
    | roomName :: sectorStr :: checksumStr :: [] -> { name = roomName; sector = Convert.ToInt32(sectorStr); checksum = checksumStr }
    | _ -> raise (Ex (sprintf "invalid room! %s" line))
;;


let rec processLines sectorSum =
    let line = Console.ReadLine() in
    if String.length line = 0 then
        sectorSum
    else
        let room = parseRoom line in
        printfn "room: %A" room;
        processLines sectorSum
;;

printfn "sum of sector IDs = %d" (processLines 0)
