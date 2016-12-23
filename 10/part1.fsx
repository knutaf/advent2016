open System;;
open System.Text.RegularExpressions;;

exception Ex of string;;

type Input =
    | Unresolved
    | Value of int
;;

type OutputConnection =
    | Unknown
    | BotNum of int
    | Output of int
;;

type Bot = { num : int; in1 : Input; in2 : Input; olow : OutputConnection; ohi : OutputConnection }
;;

let printMap map =
   List.iter (fun (k, v) -> printfn "%A -> %A" k v) (Map.toList map)
;;

let rexmatch pat str =
    let m = Regex.Match(str, pat) in
    if m.Groups.Count = 0 then
        []
    else
        List.tail [ for g in m.Groups -> g.Value ]
;;

let parseBotInput line =
    match (rexmatch @"^value (\d+) goes to bot (\d+)$" line) with
    | [valueStr; botStr] -> Some (Convert.ToInt32(botStr), Value(Convert.ToInt32(valueStr)))
    | _ -> None
;;

let parseBotOutputs line =
    let outputConnectionFromStrings outType (outStr:string) =
        if outType = "output" then
            Output (Convert.ToInt32(outStr))
        elif outType = "bot" then
            BotNum (Convert.ToInt32(outStr))
        else
            raise (Ex (sprintf "unknown output type! %s" outType))
    in
    match (rexmatch @"^bot (\d+) gives low to (output|bot) (\d+) and high to (output|bot) (\d+)$" line) with
    | [botStr; outLowType; outLowNum; outHighType; outHigNum] ->
        Some (
            Convert.ToInt32(botStr),
            outputConnectionFromStrings outLowType outLowNum,
            outputConnectionFromStrings outHighType outHigNum
            )
    | _ -> None
;;

let addInputToBot botMap input botNum =
    let (newBot, didAdd) =
        match Map.tryFind botNum botMap with
        | Some bot ->
            let _ = assert (bot.num = botNum) in
            if bot.in1 = Unresolved then
                let _ = assert (bot.in2 = Unresolved) in
                ({ num = bot.num; in1 = input; in2 = bot.in2; olow = bot.olow; ohi = bot.ohi }, true)
            elif bot.in2 = Unresolved then
                ({ num = bot.num; in1 = bot.in1; in2 = input; olow = bot.olow; ohi = bot.ohi }, true)
            else
                (bot, false)
        | None -> ({ num = botNum; in1 = input; in2 = Unresolved; olow = Unknown; ohi = Unknown }, true)
    in
    (Map.add botNum newBot botMap, didAdd)
;;

let addOutputsToBot botMap outLow outHigh botNum =
    let newBot =
        match Map.tryFind botNum botMap with
        | Some bot ->
            let _ = assert (bot.num = botNum) in
            let _ = assert(bot.olow = Unknown) in
            let _ = assert(bot.ohi = Unknown) in
            { num = bot.num; in1 = bot.in1; in2 = bot.in2; olow = outLow; ohi = outHigh }
        | None -> { num = botNum; in1 = Unresolved; in2 = Unresolved; olow = outLow; ohi = outHigh }
    in
    Map.add botNum newBot botMap
;;

let rec processLines botMap =
    let line = Console.ReadLine() in
    if String.length line = 0 then
        botMap
    else
        let newBotMap =
            match parseBotInput line with
            | Some (botNum, input) -> fst (addInputToBot botMap input botNum)
            | None ->
                match parseBotOutputs line with
                | Some (botNum, outLow, outHigh) -> addOutputsToBot botMap outLow outHigh botNum
                | None -> raise (Ex (sprintf "unrecognized line! %s" line))
        in
        processLines newBotMap
in
;;

let checkBotMapForAllOutputs botMap =
   Map.exists (fun _ bot -> bot.olow = Unknown || bot.ohi = Unknown) botMap
;;

let checkBotMapForAllInputs botMap =
   Map.exists (fun _ bot -> bot.in1 = Unresolved || bot.in2 = Unresolved) botMap
;;

let resolveInputs botMap =
    let trySetConnectedInputs (botMap, didResolve) _ bot =
        let trySetConnectedInput outputConnection value botMap =
            match outputConnection with
            | BotNum (outputBotNum) -> addInputToBot botMap (Value(value)) outputBotNum
            | Output (_) -> (botMap, false)
            | Unknown -> raise (Ex (sprintf "bot has unknown output! %A" bot))
        in
        match (bot.in1, bot.in2) with
        | (Value(val1), Value(val2)) ->
            let (botMap2, didResolve2) = trySetConnectedInput bot.olow (min val1 val2) botMap in
            let (botMap3, didResolve3) = trySetConnectedInput bot.ohi (max val1 val2) botMap2 in
            (botMap3, didResolve || didResolve2 || didResolve3)
        | _ -> (botMap, didResolve)
    in
    Map.fold trySetConnectedInputs (botMap, false) botMap
;;

let rec resolveInputsUntilDone botMap =
    let (botMap2, didResolve) = resolveInputs botMap in
    if didResolve then
        resolveInputsUntilDone botMap2
    else
        botMap
;;

[<EntryPoint>]
let main argv =
    try
        let findInput1 = Convert.ToInt32(argv.[0]) in
        let findInput2 = Convert.ToInt32(argv.[1]) in
        let botMap = processLines Map.empty in
        let _ = printMap botMap in
        let _ = printfn "check any output unset: %A" (checkBotMapForAllOutputs botMap) in
        let botMapResolved = resolveInputsUntilDone botMap in
        let _ = printMap botMapResolved in
        let _ = printfn "check any input unresolved: %A" (checkBotMapForAllInputs botMapResolved) in
        let targetBotNum = Map.findKey (fun _ bot -> (bot.in1 = Value(findInput1) && bot.in2 = Value(findInput2)) || (bot.in1 = Value(findInput2) && bot.in2 = Value(findInput1))) botMapResolved in
        printfn "bot %d matches inputs %d and %d" targetBotNum findInput1 findInput2
    with ex -> printfn "Exception: %A" ex; raise ex
    0
;;
