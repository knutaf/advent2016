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

let indexOf ch str =
    try
        Seq.findIndex (fun elem -> elem = ch) str
    with _ ->
       -1
;;

let rec supportsTLS foundABBA inBrackets addr =
    if (String.length addr) < 4 then
        foundABBA
    else
        let firstFour = addr.[.. 3] in
        let matchesABBA =
            match rexmatch "^(\\w)(\\w)\\2\\1$" firstFour with
            | [] -> false
            | [a; b] when a = b -> false
            | _ -> true
        in
        if matchesABBA then
            if inBrackets then
                false
            else
                supportsTLS true inBrackets addr.[4 ..]
        else
            let nextBracket =
                if inBrackets then
                    ']'
                else
                    '['
            in
            let indexOfBracket = indexOf nextBracket firstFour in
            if indexOfBracket <> -1 then
                supportsTLS foundABBA (not inBrackets) addr.[(indexOfBracket + 1) ..]
            else
                supportsTLS foundABBA inBrackets addr.[1 ..]
;;

let rec processLines numSupporting =
    let line = Console.ReadLine() in
    if String.length line = 0 then
        numSupporting
    else
        let nextNumSupporting =
            let supports = supportsTLS false false line in
            (*let _ = printfn "%s: %A" line supports in*)
            if supports then
                numSupporting + 1
            else
                numSupporting
        in
        processLines nextNumSupporting
;;

printfn "num supporting TLS: %d" (processLines 0)
