open System;;

exception Ex of string;;

type 'a tup =
    | Tuple of 'a list
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
        (*printfn "%sgetEachNHelper %d %A" indent n ls;*)
        if n > 1 then
            if (List.length ls) < n then
                []
            elif (List.length ls) = n then
                [Tuple ls]
            else
                let sublists = getAllSublists 1 ls in
                let nMinus1TupleLists = List.map (getEachNHelper (indent + "  ") origN (n - 1)) sublists in
                let nMinus1Tuples = List.fold (fun sofar tupleList -> sofar @ tupleList) [] nMinus1TupleLists in
                (*let _ = printfn "%snMinus1Tuples = %A" indent nMinus1Tuples in*)
                let nLists = List.map (fun (Tuple t) -> Tuple ((List.head ls) :: t)) nMinus1Tuples in
                (*let _ = printfn "%snLists = %A" indent nLists in*)
                if n = origN then
                    nLists @ (getEachNHelper (indent + "  ") origN n (List.tail ls))
                else
                    nLists
        else
            [Tuple [List.head ls]]
    in
    let tuples = getEachNHelper "" n n ls in
    let _ = List.iter (fun (Tuple (t)) -> assert ((List.length t) = n)) in
    tuples
;;

let NUM_ELEMENTS = 5;;
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

type State = { floors : Set<Slot>[]; elevatorFloor : int; numMoves : int };;
type HistoryEntry = { state : State; hash : uint64 };;

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
    for floorNum = ((Array.length state.floors) - 1) downto 0 do printFloor floorNum
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

let createHistoryEntry state =
    { state = state; hash = calculateHash state }
;;

let tryPlacing (floors : Set<Slot>[]) elevatorFloor r selectedSlots =
    let checkFloor floorSet =
        let floorHasAnyGen = Set.exists isGen floorSet in
        Set.fold (fun errorMessage slot ->
            if errorMessage = "" then
                if hasPairedSlotOnFloor slot floorSet then
                    ""
                elif (isMic slot) && floorHasAnyGen then
                    sprintf "slot %A is unsafe" slot
                else
                    ""
            else
                errorMessage
            ) "" floorSet
    in
    let slotsList = Set.toList selectedSlots in
    let sourceFloorSet = List.fold (fun floorSet slot -> Set.remove slot floorSet) floors.[elevatorFloor] slotsList in
    let destFloorSet = List.fold (fun floorSet slot -> Set.add slot floorSet) floors.[r] slotsList in
    let errorMessage =
        let sourceErrorMessage = checkFloor sourceFloorSet in
        if sourceErrorMessage = "" then
            checkFloor destFloorSet
        else
            sourceErrorMessage
    in
    if errorMessage = "" then
        let newFloors = Array.copy floors in
        let _ = newFloors.[elevatorFloor] <- sourceFloorSet in
        let _ = newFloors.[r] <- destFloorSet in
        (newFloors, r, Set.empty<Slot>, errorMessage)
    else
        (floors, elevatorFloor, selectedSlots, errorMessage)
;;

let INITIAL_STATE =
    {
        floors =
            [|
                Set.ofList [ Gen P; Mic P ];
                Set.ofList [ Gen C; Gen B; Gen R; Gen F ];
                Set.ofList [ Mic C; Mic B; Mic R; Mic F ];
                Set.empty<Slot>
            |];
        elevatorFloor = 0;
        numMoves = 0;
    }
;;

let INITIAL_HASH = calculateHash INITIAL_STATE;;

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

let FINAL_HASH = calculateHash FINAL_STATE;;

let bfs history frontier =
    match frontier with
    | [] -> raise (Ex "empty frontier!")
    | nextEntryToExpand :: restOfFrontier ->
        let expandEntries entry =
        in
        let expandedEntries = expandEntries nextEntryToExpand in
        bfs (nextEntryToExpand :: history) (restOfFrontier @ expandEntries)
;;

let finalHistoryEntry = bfs [] [ createHistoryEntry INITIAL_STATE ] in
drawState finalHistoryEntry.state
;;
