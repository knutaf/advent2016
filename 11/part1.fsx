open System;;

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

let drawGridWithCursor (floors : Set<Slot>[]) elevatorFloor numMoves r c selectedSlots =
    Console.SetCursorPosition(0, 0);
    let printFloor floorNum =
        let floorSet = floors.[floorNum] in
        let printSlot slotNum slot =
            cprintf
                (if floorNum = elevatorFloor && Set.contains slot selectedSlots then Some ConsoleColor.Red else None)
                (if floorNum = r && slotNum = c then Some ConsoleColor.DarkBlue else None)
                "%2s "
                (if Set.contains slot floorSet then (string_from_slot slot) else ".")
        in
        let _ = printf "F%d %s  " (floorNum + 1) (if elevatorFloor = floorNum then "E" else ".") in
        let _ = Array.iteri printSlot ORDERED_SLOTS in
        printfn ""
    in
    let _ = printfn "Moves: %d" numMoves in
    for floorNum = ((Array.length floors) - 1) downto 0 do printFloor floorNum
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

let tryPlacing (floors : Set<Slot>[]) elevatorFloor r c selectedSlots =
    (floors, r, Set.empty<Slot>)
;;

let rec processNextKey floors elevatorFloor numMoves r c selectedSlots =
    let isCarrying = not (Set.isEmpty selectedSlots) in
    let moveCursor r c key =
        match key with
        | ConsoleKey.Spacebar ->
            let (newFloors, newElevatorFloor, newSelectedSlots) =
                if r = elevatorFloor then
                    (floors, elevatorFloor, tryToggleSlotSelected floors elevatorFloor r c selectedSlots)
                else
                    tryPlacing floors elevatorFloor r c selectedSlots
            in
            (
            newFloors,
            newElevatorFloor,
            r,
            c,
            (if elevatorFloor <> newElevatorFloor then numMoves + 1 else numMoves),
            newSelectedSlots
            )
        | ConsoleKey.RightArrow ->
            (
            floors,
            elevatorFloor,
            r,
            min (c + 1) ((Array.length ORDERED_SLOTS) - 1),
            numMoves,
            selectedSlots
            )
        | ConsoleKey.LeftArrow ->
            (
            floors,
            elevatorFloor,
            r,
            max (c - 1) 0,
            numMoves,
            selectedSlots
            )
        | ConsoleKey.UpArrow ->
            (
            floors,
            elevatorFloor,
            List.min [r + 1; elevatorFloor + (if isCarrying then 1 else 0); (Array.length INITIAL_FLOORS) - 1],
            c,
            numMoves,
            selectedSlots
            )
        | ConsoleKey.DownArrow ->
            (
            floors,
            elevatorFloor,
            List.max [r - 1; elevatorFloor - (if isCarrying then 1 else 0); 0],
            c,
            numMoves,
            selectedSlots
            )
        | _ -> (floors, elevatorFloor, r, c, numMoves, selectedSlots)
    in
    let _ = drawGridWithCursor floors elevatorFloor numMoves r c selectedSlots in
    let keyInfo = Console.ReadKey(true) in
    let (newFloors, newElevatorFloor, newR, newC, newNumMoves, newSelectedSlots) = moveCursor r c keyInfo.Key in
    processNextKey newFloors newElevatorFloor newNumMoves newR newC newSelectedSlots
;;

Console.CursorVisible <- false;
Console.Clear();

processNextKey INITIAL_FLOORS 0 0 0 0 Set.empty<Slot>
;;
