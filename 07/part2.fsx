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

let findABA str =
    match rexmatch "^(\\w)(\\w)\\1$" str with
    | [] -> None
    | [a; b] when a <> b -> Some (a, b)
    | _ -> None
;;

let ensureInList elem ls =
    if (List.exists (fun e -> elem = e) ls) then
        ls
    else
        elem :: ls
;;

let isAnyAInB listA listB =
    List.exists (fun a -> List.exists (fun b -> a = b) listB) listA
;;

let rec supportsSSL abas babs inBrackets addr =
    if (String.length addr) < 3 then
        isAnyAInB abas babs
    else
        let firstThree = addr.[.. 2] in
        match findABA firstThree with
        | Some (a, b) ->
            (*printfn "found %s%s%s" a b a;*)
            if inBrackets then
                supportsSSL abas (ensureInList (b, a) babs) inBrackets addr.[1 ..]
            else
                supportsSSL (ensureInList (a, b) abas) babs inBrackets addr.[1 ..]
        | None ->
            let nextBracket =
                if inBrackets then
                    ']'
                else
                    '['
            in
            let indexOfBracket = indexOf nextBracket firstThree in
            if indexOfBracket <> -1 then
                supportsSSL abas babs (not inBrackets) addr.[(indexOfBracket + 1) ..]
            else
                supportsSSL abas babs inBrackets addr.[1 ..]
;;

let rec processLines numSupporting =
    let line = Console.ReadLine() in
    if String.length line = 0 then
        numSupporting
    else
        let nextNumSupporting =
            let supports = supportsSSL [] [] false line in
            (*let _ = printfn "%s: %A" line supports in*)
            if supports then
                numSupporting + 1
            else
                numSupporting
        in
        processLines nextNumSupporting
;;

printfn "num supporting SSL: %d" (processLines 0)
