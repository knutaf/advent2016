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

let string_of_char (ch : char) =
    Convert.ToString(ch)
;;

let rec processLines maps line =
    if String.length line = 0 then
        maps
    else
        assert ((String.length line) = (Array.length maps));
        let addLetterToMap map letter =
            let newCount =
                match Map.tryFind letter map with
                | None -> 1
                | Some count -> count + 1
            in
            (*let _ = printfn "adding %c:%d to map" letter newCount in*)
            Map.add letter newCount map
        in
        let newMaps = Array.mapi (fun i elem -> addLetterToMap elem line.[i]) maps in
        processLines newMaps (Console.ReadLine())
in
let line = Console.ReadLine() in
let maps = [| for i in 1 .. (String.length line) -> Map.empty |] in
let filledMaps = processLines maps line in
(*let _ = Array.mapi (fun i elem -> printfn "map %d: %A" i elem) filledMaps in*)
let message = Array.fold (fun sofar map -> sofar + (string_of_char (List.head (List.map fst (List.sortWith compareLetterCounts (Map.toList map)))))) "" filledMaps in
printfn "message: %s" message
;;
