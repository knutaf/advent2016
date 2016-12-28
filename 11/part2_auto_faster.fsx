open System;;

exception Ex of string;;

let LOG_LEVEL = 3 in
let dbglog level fmt =
    let printer (s : string) =
        if level <= LOG_LEVEL then
            let _ = System.Console.WriteLine s in
            ()
        else
            ()
    in
    Printf.kprintf printer fmt
;;

let rec getAllSublists minLength ls =
    if (List.length ls) > minLength then
        match ls with
        | [] -> []
        | head :: [] -> []
        | head :: tail -> tail :: (getAllSublists minLength tail)
    else
        []
;;

let rec getEachN n ls =
    let rec getEachNHelper indent origN n ls =
//        dbglog 5 "%sgetEachNHelper %d %A" indent n ls;
        if n > 1 then
            if (List.length ls) < n then
                []
            elif (List.length ls) = n then
                [ls]
            else
                let sublists = getAllSublists 1 ls in
                let nMinus1TupleLists = List.map (getEachNHelper (indent + "  ") origN (n - 1)) sublists in
                let nMinus1Tuples = List.fold (fun sofar tupleList -> sofar @ tupleList) [] nMinus1TupleLists in
//                let _ = dbglog 5 "%snMinus1Tuples = %A" indent nMinus1Tuples in
                let nLists = List.map (fun t -> (List.head ls) :: t) nMinus1Tuples in
//                let _ = dbglog 5 "%snLists = %A" indent nLists in
                if n = origN then
                    nLists @ (getEachNHelper (indent + "  ") origN n (List.tail ls))
                else
                    nLists
        elif n <> origN then
            [[List.head ls]]
        else
            List.map (fun elem -> [elem]) ls
    in
    let tuples = getEachNHelper "" n n ls in
    let _ = List.iter (fun t -> assert ((List.length t) = n)) in
    tuples
;;

let MAX_CARRY = 2;;

type Element =
| P
| C
| B
| R
| F
| E
| D
;;

type Slot =
| Gen of Element
| Mic of Element
;;

type State = { floors : Set<Slot>[]; elevatorFloor : int; numMoves : int };;
type HistoryEntry = { state : State; parent : HistoryEntry option; hash : uint64 };;
type FrontierNode = { entry : HistoryEntry; mutable next : FrontierNode option };;

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
        Gen E;
        Mic E;
        Gen D;
        Mic D;
    |]
;;

let INITIAL_STATE =
    {
        floors =
            [|
                Set.ofList [ Gen P; Mic P; Gen E; Mic E; Gen D; Mic D ];
                Set.ofList [ Gen C; Gen B; Gen R; Gen F ];
                Set.ofList [ Mic C; Mic B; Mic R; Mic F ];
                Set.empty<Slot>
            |];
        elevatorFloor = 0;
        numMoves = 0;
    }
;;

(*
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

let INITIAL_STATE =
    {
        floors =
            [|
                Set.ofList [ Gen P; Mic P; ];
                Set.ofList [ Gen C; Gen B; Gen R; Gen F ];
                Set.ofList [ Mic C; Mic B; Mic R; Mic F ];
                Set.empty<Slot>
            |];
        elevatorFloor = 0;
        numMoves = 0;
    }
;;
*)

(* test data
let ORDERED_SLOTS =
    [|
        Gen P;
        Mic P;
        Gen B;
        Mic B;
    |]
;;

let INITIAL_STATE =
    {
        floors =
            [|
                Set.ofList [ Mic P; Mic B ];
                Set.ofList [ Gen P; ];
                Set.ofList [ Gen B; ];
                Set.empty<Slot>
            |];
        elevatorFloor = 0;
        numMoves = 0;
    }
;;
*)

(*
let ORDERED_SLOTS =
    [|
        Gen P;
        Mic P;
        Gen B;
        Mic B;
        Gen C;
        Mic C;
    |]
;;

let INITIAL_STATE =
    {
        floors =
            [|
                Set.ofList [ Mic P; Mic B; ];
                Set.ofList [ Gen P; Gen C; Mic C; ];
                Set.ofList [ Gen B; ];
                Set.empty<Slot>
            |];
        elevatorFloor = 0;
        numMoves = 0;
    }
;;
*)

let FINAL_STATE =
    {
        floors =
            [|
                Set.empty<Slot>;
                Set.empty<Slot>;
                Set.empty<Slot>;
                Set.ofArray ORDERED_SLOTS
            |];
        elevatorFloor = 3;
        numMoves = 0;
    }
;;

let string_from_slot slot =
    let string_from_element element =
        match element with
        | P -> "P"
        | C -> "C"
        | B -> "B"
        | R -> "R"
        | F -> "F"
        | E -> "E"
        | D -> "D"
    in
    match slot with
    | Gen(e) -> (string_from_element e) + "G"
    | Mic(e) -> (string_from_element e) + "M"
;;

let drawHistoryEntry entry =
    let printFloor floorNum =
        let floorSet = entry.state.floors.[floorNum] in
        let printSlot slotNum slot =
            printf
                "%2s "
                (if Set.contains slot floorSet then (string_from_slot slot) else ".")
        in
        let _ = printf "F%d %s  " (floorNum + 1) (if entry.state.elevatorFloor = floorNum then "E" else ".") in
        let _ = Array.iteri printSlot ORDERED_SLOTS in
        printfn ""
    in
    let _ = printfn "Moves: %d. Hash: %u" entry.state.numMoves entry.hash in
    let _ = (for floorNum = ((Array.length entry.state.floors) - 1) downto 0 do printFloor floorNum) in
    printfn ""
;;

let dbgDrawHistoryEntry level entry =
    if level <= LOG_LEVEL then
        drawHistoryEntry entry
    else
        ()
;;

let rec dbgDrawFrontier level node =
    match node.next with
    | None -> ()
    | Some next ->
        let _ = dbgDrawHistoryEntry level node.entry in
        dbgDrawFrontier level next
;;

let rec drawHistory historyEntry =
    drawHistoryEntry historyEntry;
    match historyEntry.parent with
    | None -> ()
    | Some parentEntry -> drawHistory parentEntry
;;

let elemFromSlot slot =
    match slot with
    | Gen g -> g
    | Mic m -> m
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

let hasPairedSlotOnFloor slot floorSet =
    match slot with
    | Gen g -> Set.contains (Mic g) floorSet
    | Mic m -> Set.contains (Gen m) floorSet
;;

let calculateHash state =
    let addElementsFromFloor (seenElements, numElementsSeen) floorSet =
        let addElement (seenElements, numElementsSeen) slot =
            let elem = elemFromSlot slot in
            if (Map.tryFind elem seenElements).IsNone then
                (Map.add elem numElementsSeen seenElements, numElementsSeen + 1)
            else
                (seenElements, numElementsSeen)
        in
        Set.fold addElement (seenElements, numElementsSeen) floorSet
    in
    let (seenElements, _) = Array.fold addElementsFromFloor (Map.empty, 0) state.floors in
//    let _ = dbglog 5 "seen elements: %A" seenElements in
    let addFloorToHash hashSofar floorNum =
//        let _ = dbglog 5 "addFloorToHash %u %d" hashSofar floorNum in
        let addSlotToHash hashSofar slot =
//            let _ = dbglog 5 "addSlotToHash %u %A" hashSofar slot in
            let bitPosition =
                (* +2 is for the elevator value *)
                2 +
                floorNum * (Array.length ORDERED_SLOTS) +
                (((Map.tryFind (elemFromSlot slot) seenElements).Value) *
                 (if isGen slot then 2 else 1))
            in
            let _ = assert (bitPosition < 64) in
            hashSofar ||| (1UL <<< bitPosition)
        in
        Set.fold addSlotToHash hashSofar state.floors.[floorNum]
    in
    let addElevatorBitToHash hashSofar =
        let _ = assert ((hashSofar &&& 3UL) = 0UL) in
        hashSofar ||| (uint64 state.elevatorFloor)
    in
    let hashSoFar = Seq.fold addFloorToHash 0UL (seq { 0 .. ((Array.length state.floors) - 1)}) in
    addElevatorBitToHash hashSoFar
;;

let createHistoryEntry state parent =
    { state = state; parent = parent; hash = calculateHash state }
;;

let compareHistoryEntriesForFrontier entry1 entry2 =
    if entry1.state.numMoves = entry2.state.numMoves then
        if entry1.hash = entry2.hash then
            0
        elif entry1.hash > entry2.hash then
            -1
        else
            1
    elif entry1.state.numMoves < entry2.state.numMoves then
        -1
    else
        1
;;

let rec frontierLength frontier =
    let rec helper lengthSofar node =
        match node.next with
        | None -> lengthSofar + 1
        | Some next -> helper (lengthSofar + 1) next
    in
    helper 0 frontier
;;

let addHistoryEntryToList entry frontier =
    let rec insertSortedAfter node =
        match node.next with
        | None ->
            node.next <- Some { entry = entry; next = None }
        | Some next ->
            let comparison = compareHistoryEntriesForFrontier entry next.entry in
            if comparison = 0 then
                ()
            elif comparison < 0 then
                let newNode = { entry = entry; next = node.next } in
                node.next <- Some newNode
            else
                insertSortedAfter next
    in
    let headComparison = compareHistoryEntriesForFrontier entry frontier.entry in
    if headComparison < 0 then
        { entry = entry; next = Some frontier }
    elif headComparison = 0 then
        frontier
    else
        let _ = insertSortedAfter frontier in
        frontier
;;

let isFloorSafe floorSet =
    let floorHasAnyGen = Set.exists isGen floorSet in
    Set.fold (fun isValid slot ->
        if isValid then
            if hasPairedSlotOnFloor slot floorSet then
                isValid
            elif (isMic slot) && floorHasAnyGen then
                false
            else
                isValid
        else
            isValid
        ) true floorSet
;;

let expandEntries history frontier =
    let entry = frontier.entry in
    //let _ = dbglog 3 "expanding:" in
    //let _ = dbgDrawHistoryEntry 3 entry in
    let slotsOnFloor = Set.toList entry.state.floors.[entry.state.elevatorFloor] in
    let addEntryIfValid elevatorOffset frontier slotsToMove =
        assert (elevatorOffset = 1 || elevatorOffset = -1);
        let elevatorFloorAfterMove = entry.state.elevatorFloor + elevatorOffset in
        if elevatorFloorAfterMove >= 0 && elevatorFloorAfterMove < (Array.length entry.state.floors) then
            let sourceFloorSet = List.fold (fun floorSet slot -> Set.remove slot floorSet) entry.state.floors.[entry.state.elevatorFloor] slotsToMove in
            if isFloorSafe sourceFloorSet then
                let destFloorSet = List.fold (fun floorSet slot -> Set.add slot floorSet) entry.state.floors.[elevatorFloorAfterMove] slotsToMove in
                if isFloorSafe destFloorSet then
                    let floorsAfterMove = Array.copy entry.state.floors in
                    let _ = floorsAfterMove.[entry.state.elevatorFloor] <- sourceFloorSet in
                    let _ = floorsAfterMove.[elevatorFloorAfterMove] <- destFloorSet in
                    let newHistoryEntry = createHistoryEntry { floors = floorsAfterMove; elevatorFloor = elevatorFloorAfterMove; numMoves = entry.state.numMoves + 1 } (Some entry) in
                    if not (Set.contains newHistoryEntry.hash history) then
                        //let _ = dbglog 3 "adding to frontier: %A %d" (List.map string_from_slot slotsToMove) elevatorOffset in
                        //let _ = dbgDrawHistoryEntry 3 newHistoryEntry in
                        addHistoryEntryToList newHistoryEntry frontier
                    else
                       frontier
                else
                    frontier
            else
                frontier
        else
            frontier
    in
    let allCombinationsOnFloor = Seq.fold (fun sets carry -> sets @ (getEachN carry slotsOnFloor)) [] (seq { 1 .. MAX_CARRY }) in
    //let _ = dbglog 3 "all combos: %A" allCombinationsOnFloor in
    let frontierWithMinusOneEntries = List.fold (addEntryIfValid -1) frontier allCombinationsOnFloor in
    let newFrontier = List.fold (addEntryIfValid 1) frontierWithMinusOneEntries allCombinationsOnFloor in
    //let _ = dbglog 3 "expanded frontier is:" in
    //let _ = dbgDrawFrontier 3 newFrontier in
    newFrontier.next.Value
;;

let FINAL_ENTRY = createHistoryEntry FINAL_STATE None;;

let rec bfs iterations history frontier =
    let _ =
        if (iterations % 1000UL) = 0UL then
            //printfn "historySize %d, frontierSize %d" (Set.count history) (frontierLength frontier)
            printfn "historySize %d. numMoves here: %d. hash here: %u" (Set.count history) frontier.entry.state.numMoves frontier.entry.hash
        else
            ()
    in
    let nextEntryToExpand = frontier.entry in
    if nextEntryToExpand.hash = FINAL_ENTRY.hash then
        nextEntryToExpand
    elif not (Set.contains nextEntryToExpand.hash history) then
        let expandedFrontier = expandEntries history frontier in
        bfs (iterations + 1UL) (Set.add nextEntryToExpand.hash history) expandedFrontier
    else
        bfs (iterations + 1UL) history frontier.next.Value
;;

let finalHistoryEntry = bfs 0UL Set.empty {entry = createHistoryEntry INITIAL_STATE None; next = None }
drawHistory finalHistoryEntry
;;

(*
let expanded = (expandEntries [] [] (createHistoryEntry INITIAL_STATE)) in
List.iter (fun entry -> drawHistoryEntry entry.state) expanded
;;
*)
