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

let compareLetterCounts a b =
    if (snd a) < (snd b) then
        1
    elif (snd a) > (snd b) then
        -1
    elif (fst a) > (fst b) then
        1
    elif (fst a) < (fst b) then
        -1
    else
        0
;;

type room = { name : string; sector : int; checksum : string };;

let parseRoom line =
    let parts = rexmatch "(.*)-(\d+)\\[(\w+)\\]$" line in
    match parts with
    | roomName :: sectorStr :: checksumStr :: [] -> { name = roomName; sector = Convert.ToInt32(sectorStr); checksum = checksumStr }
    | _ -> raise (Ex (sprintf "invalid room! %s" line))
;;

let isRealRoom room =
    let addLetter (letterMap : Map<char, int>) letter =
        if letter <> '-' then
            let newCount =
                match Map.tryFind letter letterMap with
                | None -> 1
                | Some count -> count + 1
            in
            Map.add letter newCount letterMap
        else
            letterMap
    in
    let letterMap = Seq.fold addLetter Map.empty room.name in
    let topLetters = List.map (fun (a, b) -> a) (List.sortWith compareLetterCounts (Map.toList letterMap)) in
    let calculatedChecksum =
        match topLetters with
        | a :: b :: c :: d :: e :: rest -> Convert.ToString(a) + Convert.ToString(b) + Convert.ToString(c) + Convert.ToString(d) + Convert.ToString(e)
        | _ -> raise (Ex (sprintf "invalid room doesn't have 5 top letters! %A" topLetters))
    in
    (*printfn "calc: %s, actual: %s" calculatedChecksum room.checksum;*)
    calculatedChecksum = room.checksum
;;

let decryptRoom room =
    let rec rot num c =
        if c = '-' then
            ' '
        elif num > 0 then
            if c = 'z' then
                rot (num - 1) 'a'
            else
                rot (num - 1) (Convert.ToChar ((Convert.ToInt32 c) + 1))
        else
            c
    in
    String.map (fun elem -> rot room.sector elem) room.name
;;

let rec processLines () =
    let line = Console.ReadLine() in
    if String.length line = 0 then
        ()
    else
        let room = parseRoom line in
            if isRealRoom room then
                printfn "%s-%d decrypted: %s" room.name room.sector (decryptRoom room)
            else
                ()
        ;
        processLines ()
;;

processLines ();;
