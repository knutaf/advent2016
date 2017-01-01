open System;;

exception Ex of string;;

let countBits value =
    let incIfBit sofar bitPos =
        if (value &&& (1UL <<< bitPos)) = 0UL then
            sofar
        else
            sofar + 1
    in
    Seq.fold incIfBit 0 (seq { 0 .. 63 })
;;

let isOpen favoriteNumber (x, y) =
    let ui64X = uint64 x in
    let ui64Y = uint64 y in
    let calc = (ui64X * ui64X) + (3UL * ui64X) + (2UL * ui64X * ui64Y) + ui64Y + (ui64Y * ui64Y) + (uint64 favoriteNumber) in
    ((countBits calc) % 2) = 0
;;

let printGrid favoriteNumber sizeX sizeY =
    let printRow y =
        printf "%-2d " y;
        for x = 0 to (sizeX - 1) do printf "%2s " (if (isOpen favoriteNumber (x, y)) then "." else "#");
        printfn ""
    in
    printf "   ";
    for x = 0 to (sizeX - 1) do printf "%2d " x;
    printfn "";
    for y = 0 to (sizeY - 1) do printRow y;
    printfn "";
;;

type State = { pos : int * int; numMoves : int };;
type HistoryEntry = { state : State; parent : HistoryEntry option };;

let expandEntry favoriteNumber history frontier entry =
    let moveOffsets = [(-1, 0); (0, -1); (1, 0); (0, 1)] in
    let addOffsetIfValid sofar offset =
        let newPos = ((fst entry.state.pos) + (fst offset), ((snd entry.state.pos) + (snd offset))) in
        if (fst newPos) >= 0 && (snd newPos) >= 0 && (isOpen favoriteNumber newPos) then
            if not (Set.contains newPos history) then
                if (List.tryFind (fun elem -> elem.state.pos = newPos) frontier).IsNone then
                    { state = { pos = newPos; numMoves = entry.state.numMoves + 1 }; parent = Some entry } :: sofar
                else
                    sofar
            else
                sofar
        else
            sofar
    in
    List.fold addOffsetIfValid [] moveOffsets
;;

let rec bfs favoriteNumber targetPosition iterations history frontier =
    let _ =
        if (iterations % 1) = 0 then
            printfn "historySize %d, frontierSize %d, numMoves here %d" (Set.count history) (List.length frontier) (List.head frontier).state.numMoves
        else
            ()
    in
    match frontier with
    | [] ->
        printfn "history: %A" history;
        raise (Ex "empty frontier!")
    | nextEntryToExpand :: restOfFrontier ->
        let expandedEntries = expandEntry favoriteNumber history frontier nextEntryToExpand in
        let maybeFinalEntry = List.tryFind (fun elem -> elem.state.pos = targetPosition) expandedEntries in
        match maybeFinalEntry with
        | Some finalEntry -> finalEntry
        | None -> bfs favoriteNumber targetPosition (iterations + 1) (Set.add nextEntryToExpand.state.pos history) (restOfFrontier @ expandedEntries)
;;

[<EntryPoint>]
let main argv =
    match argv with
    | [|favoriteNumberStr; destXStr; destYStr|] ->
        let favoriteNumber = Convert.ToInt32(favoriteNumberStr) in
        let destX = Convert.ToInt32(destXStr) in
        let destY = Convert.ToInt32(destYStr) in
        let finalEntry = bfs favoriteNumber (destX, destY) 0 Set.empty [{ state = { pos = (1, 1); numMoves = 0 }; parent = None }] in
        printfn "numMoves: %d" finalEntry.state.numMoves
    | _ -> printfn "need favorite number, dest x, and dest y"
    0
;;

