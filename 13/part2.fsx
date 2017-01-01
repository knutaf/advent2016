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

let drawGrid favoriteNumber sizeX sizeY path =
    let isOnPath x y =
        List.exists ((=) (x, y)) path
    in
    let printRow y =
        let getStrForPos x y =
            if (isOnPath x y) then
                "O"
            elif (isOpen favoriteNumber (x, y)) then
                "."
            else
                "#"
        in
        printf "%-2d " y;
        for x = 0 to sizeX do printf "%2s " (getStrForPos x y);
        printfn ""
    in
    printf "   ";
    for x = 0 to sizeX do printf "%2d " x;
    printfn "";
    for y = 0 to sizeY do printRow y;
    printfn "";
;;

type State = { pos : int * int; numMoves : int };;
type HistoryEntry = { state : State; parent : HistoryEntry option };;

let drawHistory favoriteNumber entry =
    let rec getMaxPos (maxXSoFar, maxYSoFar) entry =
        let newMaxX = max maxXSoFar (fst entry.state.pos) in
        let newMaxY = max maxYSoFar (snd entry.state.pos) in
        match entry.parent with
        | Some parent -> getMaxPos (newMaxX, newMaxY) parent
        | None -> (newMaxX, newMaxY)
    in
    let rec getPath pathSoFar entry =
        let newPath = entry.state.pos :: pathSoFar in
        match entry.parent with
        | Some parent -> getPath newPath parent
        | None -> newPath
    in
    let (maxX, maxY) = getMaxPos (0, 0) entry in
    let path = getPath [] entry in
    drawGrid favoriteNumber (maxX + 1) (maxY + 1) path
;;

let expandEntry favoriteNumber history frontier entry =
    let moveOffsets = [(-1, 0); (0, -1); (1, 0); (0, 1)] in
    let addOffsetIfValid sofar offset =
        let newPos = ((fst entry.state.pos) + (fst offset), ((snd entry.state.pos) + (snd offset))) in
        if (fst newPos) >= 0 && (snd newPos) >= 0 && (isOpen favoriteNumber newPos) then
            if not (Map.containsKey newPos history) then
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

let rec bfs favoriteNumber maxMoves iterations history frontier =
    let _ =
        if (iterations % 100) = 0 then
            printfn "historySize %d, frontierSize %d, numMoves here %d" (Map.count history) (List.length frontier) (List.head frontier).state.numMoves
        else
            ()
    in
    match frontier with
    | [] -> history
    | nextEntryToExpand :: restOfFrontier ->
        let newHistory = Map.add nextEntryToExpand.state.pos nextEntryToExpand history in
        let newFrontier =
            if nextEntryToExpand.state.numMoves < maxMoves then
                let expandedEntries = expandEntry favoriteNumber newHistory frontier nextEntryToExpand in
                restOfFrontier @ expandedEntries
            else
                let _ = assert (nextEntryToExpand.state.numMoves = maxMoves) in
                restOfFrontier
        in
        bfs favoriteNumber maxMoves (iterations + 1) newHistory newFrontier
;;

let checkHistory favoriteNumber maxMoves history =
    assert(Map.forall (fun _ entry ->
        if entry.state.numMoves <= maxMoves then
            true
        else
            let _ = printfn "too many moves! %d" entry.state.numMoves in
            let _ = drawHistory favoriteNumber entry in
            false
        )
        history)
;;

[<EntryPoint>]
let main argv =
    match argv with
    | [|favoriteNumberStr; maxMovesStr |] ->
        let favoriteNumber = Convert.ToInt32(favoriteNumberStr) in
        let maxMoves = Convert.ToInt32(maxMovesStr) in
        let finalHistory = bfs favoriteNumber maxMoves 0 Map.empty [{ state = { pos = (1, 1); numMoves = 0 }; parent = None }] in
        let _ = checkHistory favoriteNumber maxMoves finalHistory in
        printfn "historySize: %d" (Map.count finalHistory)
    | _ -> printfn "need favorite number and max moves"
    0
;;

