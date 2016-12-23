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
    | Bot of Bot
    | Output of int

and Bot = { num : int; in1 : Input; in2 : Input; olow : OutputConnection; ohi : OutputConnection }
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
    let newBot =
        match Map.tryFind botNum botMap with
        | Some bot ->
            let _ = assert (bot.num = botNum) in
            if bot.in1 = Unresolved then
                let _ = assert (bot.in2 = Unresolved) in
                { num = bot.num; in1 = input; in2 = bot.in2; olow = bot.olow; ohi = bot.ohi }
            elif bot.in2 = Unresolved then
                { num = bot.num; in1 = bot.in1; in2 = input; olow = bot.olow; ohi = bot.ohi }
            else
                raise (Ex (sprintf "bot trying to fill third input! %A" bot))
        | None -> { num = botNum; in1 = input; in2 = Unresolved; olow = Unknown; ohi = Unknown }
    in
    Map.add botNum newBot botMap
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
            | Some (botNum, input) -> addInputToBot botMap input botNum
            | None ->
                match parseBotOutputs line with
                | Some (botNum, outLow, outHigh) -> addOutputsToBot botMap outLow outHigh botNum
                | None -> raise (Ex (sprintf "unrecognized line! %s" line))
        in
        processLines newBotMap
in
let botMap = processLines Map.empty in
printMap botMap
;;
