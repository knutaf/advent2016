open System;;

exception Ex of string;;

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

type State = { floors : Set<Slot>[]; elevatorFloor : int; numMoves : int; hash : uint64 };;

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

let calculateHash floors elevatorFloor =
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
    let (seenElements, _) = Array.fold addElementsFromFloor (Map.empty, 0) floors in
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
        let newHash = Set.fold addSlotToHash hashSofar floors.[floorNum] in
        if floorNum = elevatorFloor then
            addElevatorBitToHash newHash
        else
            newHash
    in
    Seq.fold addFloorToHash 0UL (seq { 0 .. ((Array.length floors) - 1)})
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

let testFloors1 =
    [|
        Set.ofList [ Gen C; Mic C ];
        Set.ofList [ Gen P; Gen B; Gen R; Gen F ];
        Set.ofList [ Mic P; Mic B; Mic R; Mic F ];
        Set.empty<Slot>
    |]
;;

let testFloors2 =
    [|
        Set.ofList [ Gen C; Mic C ];
        Set.ofList [ Gen F; Gen R; Gen B; Gen P ];
        Set.ofList [ Mic P; Mic R; Mic B; Mic F ];
        Set.empty<Slot>
    |]
;;

let testFloors3 =
    [|
        Set.ofList [ Mic C; Gen C ];
        Set.ofList [ Gen P; Gen B; Gen R; Gen F ];
        Set.ofList [ Mic P; Mic B; Mic R; Mic F ];
        Set.empty<Slot>
    |]
;;

let testFloors4 =
    [|
        Set.ofList [ Mic C; Gen C; Gen F ];
        Set.ofList [ Gen P; Gen B; Gen R ];
        Set.ofList [ Mic P; Mic B; Mic R; Mic F ];
        Set.empty<Slot>
    |]
;;

let testFloors5 =
    [|
        Set.ofList [ Mic C; Gen C ];
        Set.ofList [ Gen P; Gen B; Gen R ];
        Set.ofList [ Mic P; Mic B; Mic R; Mic F ];
        Set.ofList [ Gen F ]
    |]
;;

let hashInit = calculateHash INITIAL_FLOORS 0 in
let testHash floors elevatorFloor =
    let hsh = calculateHash floors elevatorFloor in
    printfn "%u = %u? %A" hashInit hsh (hashInit = hsh)
in
testHash testFloors1 0;
testHash testFloors2 0;
testHash testFloors3 0;
testHash testFloors1 1;
testHash testFloors4 0;
testHash testFloors5 0;
