open System;;
open System.Text.RegularExpressions;;

exception Ex of string;;

type Marker = { length : int; times : uint64; numLeft : int };;

type InChar =
    | EOF
    | DontCare of int
    | Char of string
;;

let rexmatch pat str =
    let m = Regex.Match(str, pat) in
    if m.Groups.Count = 0 then
        []
    else
        List.tail [ for g in m.Groups -> g.Value ]
;;

let readNextChar () =
    let ch = Console.Read() in
    match ch with
    | 13 -> DontCare(ch)
    | 10 -> DontCare(ch)
    | -1 -> EOF
    | c -> Char(Convert.ToString(Convert.ToChar(c)))
;;

let rec readMarker indent markerStr =
    match readNextChar () with
    | DontCare(dcChar) -> raise (Ex (sprintf "invalid char in the middle of marker! %d %s" dcChar markerStr))
    | EOF -> raise (Ex (sprintf "EOF in the middle of marker! %s" markerStr))
    | Char(")") ->
        let fullMarkerStr = markerStr + ")" in
        let _ = printfn "%sread marker: %s" indent fullMarkerStr in
        match (rexmatch @"^\((\d+)x(\d+)\)$" fullMarkerStr) with
        | [numLeft; times] -> { length = Convert.ToInt32(String.length fullMarkerStr); numLeft = Convert.ToInt32(numLeft); times = Convert.ToUInt64(times) }
        | _ -> raise (Ex (sprintf "invalid marker! %s" fullMarkerStr))
    | Char(c) -> readMarker indent (markerStr + c)
;;

let rec processAllChars indent layerLength charsRead multiplier numLeft =
    if layerLength = 0UL then
        printfn "%sprocessAllChars %u %u %u %d" indent layerLength charsRead multiplier numLeft;
    if numLeft = 0 then
        let _ = printfn "%sret layerLength: %u, charsRead: %u" indent layerLength charsRead in
        (layerLength, charsRead)
    else
        match readNextChar () with
        | DontCare(dcChar) ->
            printfn "%swarning: invalid char! %d" indent dcChar;
            processAllChars indent layerLength charsRead multiplier numLeft
        | EOF ->
            assert (multiplier = 1UL);
            assert (numLeft < 0);
            (layerLength, charsRead)
        | Char("(") ->
            let marker = readMarker indent "(" in
            let (subLayerLength, subCharsRead) = processAllChars (indent + " ") 0UL 0 (multiplier * marker.times) marker.numLeft in
            processAllChars indent (layerLength + subLayerLength) (charsRead + marker.length + subCharsRead) multiplier (numLeft - marker.length - subCharsRead)
        | Char(c) ->
            (*printfn "char: %s" c;*)
            processAllChars indent (layerLength + multiplier) (charsRead + 1) multiplier (numLeft - 1)
;;

let (layerLength, charsRead) = processAllChars "" 0UL 0 1UL -1 in
printfn "layer length: %u, charsRead: %d" layerLength charsRead
;;
