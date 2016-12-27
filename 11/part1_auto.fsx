open System;;

exception Ex of string;;

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
        (*printfn "%sgetEachNHelper %d %A" indent n ls;*)
        if n > 1 then
            if (List.length ls) < n then
                []
            elif (List.length ls) = n then
                [ls]
            else
                let sublists = getAllSublists 1 ls in
                let nMinus1TupleLists = List.map (getEachNHelper (indent + "  ") origN (n - 1)) sublists in
                let nMinus1Tuples = List.fold (fun sofar tupleList -> sofar @ tupleList) [] nMinus1TupleLists in
                (*let _ = printfn "%snMinus1Tuples = %A" indent nMinus1Tuples in*)
                let nLists = List.map (fun t -> (List.head ls) :: t) nMinus1Tuples in
                (*let _ = printfn "%snLists = %A" indent nLists in*)
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

(*
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
*)

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

let drawState state =
    let printFloor floorNum =
        let floorSet = state.floors.[floorNum] in
        let printSlot slotNum slot =
            printf
                "%2s "
                (if Set.contains slot floorSet then (string_from_slot slot) else ".")
        in
        let _ = printf "F%d %s  " (floorNum + 1) (if state.elevatorFloor = floorNum then "E" else ".") in
        let _ = Array.iteri printSlot ORDERED_SLOTS in
        printfn ""
    in
    let _ = printfn "Moves: %d" state.numMoves in
    let _ = (for floorNum = ((Array.length state.floors) - 1) downto 0 do printFloor floorNum) in
    printfn ""
;;

let rec drawHistory historyEntry =
    drawState historyEntry.state;
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
    (*let _ = printfn "seen elements: %A" seenElements in*)
    let addFloorToHash hashSofar floorNum =
        (*let _ = printfn "addFloorToHash %u %d" hashSofar floorNum in*)
        let addSlotToHash hashSofar slot =
            (*let _ = printfn "addSlotToHash %u %A" hashSofar slot in*)
            let bitPosition =
                (* +1 is for the elevator bit *)
                floorNum * ((Array.length ORDERED_SLOTS) + 1) +
                (* *2 is for having both Gen and Mic *)
                ((Map.tryFind (elemFromSlot slot) seenElements).Value * 2) +
                (if isGen slot then 1 else 0)
            in
            hashSofar ||| (1UL <<< bitPosition)
        in
        let addElevatorBitToHash hashSofar =
            let bitPosition = floorNum * (Array.length ORDERED_SLOTS) in
            hashSofar ||| (1UL <<< bitPosition)
        in
        let newHash = Set.fold addSlotToHash hashSofar state.floors.[floorNum] in
        if floorNum = state.elevatorFloor then
            addElevatorBitToHash newHash
        else
            newHash
    in
    Seq.fold addFloorToHash 0UL (seq { 0 .. ((Array.length state.floors) - 1)})
;;

let createHistoryEntry state parent =
    { state = state; parent = parent; hash = calculateHash state }
;;

let findEquivalentHistoryEntryInList entry =
    List.tryFind (fun existingHistoryEntry -> existingHistoryEntry.hash = entry.hash)
;;

let findEquivalentHistoryEntryInMap entry =
    Map.tryFind entry.hash
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

let expandEntries history frontier entry =
    let slotsOnFloor = Set.toList entry.state.floors.[entry.state.elevatorFloor] in
    let allCombinationsOnFloor = Seq.fold (fun sets carry -> sets @ (getEachN carry slotsOnFloor)) [] (seq { 1 .. MAX_CARRY }) in
    (*let _ = printfn "all combos: %A" allCombinationsOnFloor in*)
    let addEntryIfValid elevatorOffset entriesSoFar slotsToMove =
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
                    if (findEquivalentHistoryEntryInMap newHistoryEntry history).IsNone &&
                       (findEquivalentHistoryEntryInList newHistoryEntry frontier).IsNone then
                        newHistoryEntry :: entriesSoFar
                    else
                       entriesSoFar
                else
                    entriesSoFar
            else
                entriesSoFar
        else
            entriesSoFar
    in
    let minusOneEntries = List.fold (addEntryIfValid -1) [] allCombinationsOnFloor in
    List.fold (addEntryIfValid 1) minusOneEntries allCombinationsOnFloor
;;

let FINAL_ENTRY = createHistoryEntry FINAL_STATE None;;

let rec bfs history frontier frontierSize =
    let _ =
        if ((Map.count history) % 10000) = 0 then
            printfn "historySize %d, frontierSize %d" (Map.count history) frontierSize
        else
            ()
    in
    match frontier with
    | [] ->
        printfn "history: %A" history;
        raise (Ex "empty frontier!")
    | nextEntryToExpand :: restOfFrontier ->
        let expandedEntries = expandEntries history frontier nextEntryToExpand in
        let foundFinalEntry = findEquivalentHistoryEntryInList FINAL_ENTRY expandedEntries in
        if foundFinalEntry.IsNone then
            bfs (Map.add nextEntryToExpand.hash nextEntryToExpand history) (restOfFrontier @ expandedEntries) (frontierSize + (List.length expandedEntries) - 1)
        else
            foundFinalEntry.Value
;;

let finalHistoryEntry = bfs Map.empty [ createHistoryEntry INITIAL_STATE None ] 1 in
drawHistory finalHistoryEntry
;;

(*
let expanded = (expandEntries [] [] (createHistoryEntry INITIAL_STATE)) in
List.iter (fun entry -> drawState entry.state) expanded
;;
*)
