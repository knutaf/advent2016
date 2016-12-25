open System;;

exception Ex of string;;

let cprintf fgColor bgColor fmt = 
    let printer (s : string) =
        let oldFG = System.Console.ForegroundColor in
        let oldBG = System.Console.BackgroundColor in
        let newFG =
            match fgColor with
            | None -> oldFG
            | Some color -> color
        in
        let newBG =
            match bgColor with
            | None -> oldBG
            | Some color -> color
        in
        let _ = System.Console.ForegroundColor <- newFG in
        let _ = System.Console.BackgroundColor <- newBG in
        let _ = System.Console.Write s in
        let _ = System.Console.BackgroundColor <- oldBG in
        let _ = System.Console.ForegroundColor <- oldFG in
        ()
    in
    Printf.kprintf printer fmt
;;

type Element =
| P
| C
| B
| R
| F
;;

type Slot =
| Gen of Element
| Mic of Element
;;

let MAX_CARRY = 2;;

let ORDERED_SLOTS =
    [|
        Gen P;
        Mic P;
        Gen C;
        Mic C;
        Gen B;
        Mic B;
        Gen R;
        Mic R;
        Gen F;
        Mic F;
    |]
;;

let INITIAL_FLOORS =
    [|
        Set.ofList [ Gen P; Mic P ];
        Set.ofList [ Gen C; Gen B; Gen R; Gen F ];
        Set.ofList [ Mic C; Mic B; Mic R; Mic F ];
        Set.empty<Slot>
    |]
;;

let string_from_slot slot =
    let string_from_element element =
        match element with
        | P -> "P"
        | C -> "C"
        | B -> "B"
        | R -> "R"
        | F -> "F"
    in
    match slot with
    | Gen(e) -> (string_from_element e) + "G"
    | Mic(e) -> (string_from_element e) + "M"
;;

let drawGridWithCursor (floors : Set<Slot>[]) elevatorFloor numMoves r c selectedSlots errorMessage =
    Console.SetCursorPosition(0, 0);
    let printFloor floorNum =
        let floorSet = floors.[floorNum] in
        let printSlot slotNum slot =
            cprintf
                (if floorNum = elevatorFloor && Set.contains slot selectedSlots then Some ConsoleColor.Yellow else None)
                (
                if floorNum = r then
                    if r = elevatorFloor then
                        if slotNum = c then
                            Some ConsoleColor.DarkBlue
                        else
                            None
                    elif not (Set.isEmpty selectedSlots) then
                        Some ConsoleColor.DarkBlue
                    else
                        None
                else
                    None
                )
                "%2s "
                (if Set.contains slot floorSet then (string_from_slot slot) else ".")
        in
        let _ = printf "F%d %s  " (floorNum + 1) (if elevatorFloor = floorNum then "E" else ".") in
        let _ = Array.iteri printSlot ORDERED_SLOTS in
        printfn ""
    in
    let _ = printfn "Moves: %d" numMoves in
    for floorNum = ((Array.length floors) - 1) downto 0 do printFloor floorNum;
    cprintf (Some ConsoleColor.Red) None "%-70s" errorMessage
;;

let isGen slot =
    match slot with
    | Gen _ -> true
    | Mic _ -> false
;;

let isMic slot =
    match slot with
    | Gen _ -> false
    | Mic _ -> true
;;

let tryToggleSlotSelected (floors : Set<Slot>[]) elevatorFloor r c selectedSlots =
    if r = elevatorFloor then
        let slotUnderCursor = ORDERED_SLOTS.[c] in
        let floorSet = floors.[r] in
        if Set.contains slotUnderCursor floorSet then
            if Set.contains slotUnderCursor selectedSlots then
                Set.remove slotUnderCursor selectedSlots
            elif (Set.count selectedSlots) < MAX_CARRY then
                Set.add slotUnderCursor selectedSlots
            else
                selectedSlots
        else
            selectedSlots
    else
        selectedSlots
;;

let tryPlacing (floors : Set<Slot>[]) elevatorFloor r selectedSlots =
    let hasPairedSlot slot floorSet =
        match slot with
        | Gen g -> Set.contains (Mic g) floorSet
        | Mic m -> Set.contains (Gen m) floorSet
    in
    let getErrorMessageForUnshieldedMic exceptElement floorSet =
        let checkForUnshieldedMic sofar slot =
            match slot with
            | Gen g -> sofar
            | Mic m ->
                if m = exceptElement then
                    sofar
                else
                    if not (hasPairedSlot slot floorSet) then
                        sprintf "%A missing pair" slot
                    else
                        sofar
        in
        Set.fold checkForUnshieldedMic "" floorSet
    in
    let getErrorMessageForFloorSafety floorSetForPlacing slotToPlace =
        match slotToPlace with
        | Gen g -> getErrorMessageForUnshieldedMic g floorSetForPlacing
        | Mic m ->
            if Set.contains (Gen m) floorSetForPlacing then
                ""
            elif Set.exists isGen floorSetForPlacing then
                "floor has generators, and this mic is not shielded"
            else
                ""
    in
    let tryPlaceSlot (floorSet, errorMessage) slot =
        if errorMessage = "" then
            let errorForPlacing = getErrorMessageForFloorSafety floorSet slot in
            if errorForPlacing = "" then
                (Set.add slot floorSet, errorMessage)
            else
                (floorSet, errorForPlacing)
        else
            (floorSet, errorMessage)
    in
    let slotsList = Set.toList selectedSlots in
    let (newFloorSet, errorMessage) = List.fold tryPlaceSlot (floors.[r], "") slotsList in
    if errorMessage = "" then
        let elevatorFloorSet = List.fold (fun floorSet slot -> Set.remove slot floorSet) floors.[elevatorFloor] slotsList in
        let newFloors = Array.copy floors in
        let _ = newFloors.[elevatorFloor] <- elevatorFloorSet in
        let _ = newFloors.[r] <- newFloorSet in
        (newFloors, r, Set.empty<Slot>, errorMessage)
    else
        (floors, elevatorFloor, selectedSlots, errorMessage)
;;

let rec processNextKey history numMoves r c selectedSlots errorMessage =
    let (floors, elevatorFloor) = List.head history in
    let isCarrying = not (Set.isEmpty selectedSlots) in
    let moveCursor r c key =
        match key with
        | ConsoleKey.Spacebar ->
            let (newFloors, newElevatorFloor, newSelectedSlots, newErrorMessage) =
                if r = elevatorFloor then
                    (floors, elevatorFloor, tryToggleSlotSelected floors elevatorFloor r c selectedSlots, "")
                else
                    tryPlacing floors elevatorFloor r selectedSlots
            in
            let madeMove = elevatorFloor <> newElevatorFloor in
            (
            (if madeMove then ((newFloors, newElevatorFloor) :: history) else history),
            r,
            c,
            (if madeMove then numMoves + 1 else numMoves),
            newSelectedSlots,
            newErrorMessage
            )
        | ConsoleKey.U ->
            let (newHistory, newNumMoves, newR, newC, newErrorMessage) =
                match history with
                | _ :: [] -> (history, numMoves, r, c, "No more moves to undo")
                | _ :: tail -> (tail, numMoves - 1, (snd (List.head tail)), 0, "")
                | _ -> raise (Ex "somehow got to 0 moves in history")
            in
            (
            newHistory,
            newR,
            newC,
            newNumMoves,
            selectedSlots,
            newErrorMessage
            )
        | ConsoleKey.RightArrow ->
            (
            history,
            r,
            min (c + 1) ((Array.length ORDERED_SLOTS) - 1),
            numMoves,
            selectedSlots,
            errorMessage
            )
        | ConsoleKey.LeftArrow ->
            (
            history,
            r,
            max (c - 1) 0,
            numMoves,
            selectedSlots,
            errorMessage
            )
        | ConsoleKey.UpArrow ->
            (
            history,
            List.min [r + 1; elevatorFloor + (if isCarrying then 1 else 0); (Array.length INITIAL_FLOORS) - 1],
            c,
            numMoves,
            selectedSlots,
            errorMessage
            )
        | ConsoleKey.DownArrow ->
            (
            history,
            List.max [r - 1; elevatorFloor - (if isCarrying then 1 else 0); 0],
            c,
            numMoves,
            selectedSlots,
            errorMessage
            )
        | _ -> (history, r, c, numMoves, selectedSlots, errorMessage)
    in
    let _ = drawGridWithCursor floors elevatorFloor numMoves r c selectedSlots errorMessage in
    let keyInfo = Console.ReadKey(true) in
    let (newHistory, newR, newC, newNumMoves, newSelectedSlots, newErrorMessage) = moveCursor r c keyInfo.Key in
    processNextKey newHistory newNumMoves newR newC newSelectedSlots newErrorMessage
;;

Console.CursorVisible <- false;
Console.Clear();

processNextKey [(INITIAL_FLOORS, 0)] 0 0 0 Set.empty<Slot> ""
;;
