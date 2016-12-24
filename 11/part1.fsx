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

let ORDERED_SLOTS =
    [
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
    ]
;;

let drawGridWithCursor (floors : Set<Slot>[]) elevatorFloor r c =
    Console.SetCursorPosition(0, 0);
    let printFloor floorNum =
        let fl = floors.[floorNum] in
        let printSlot slotNum slot =
            cprintf
                None
                (if floorNum = r && slotNum = c then Some ConsoleColor.DarkBlue else None)
                "%2s "
                (if Set.contains slot fl then (string_from_slot slot) else ".")
        in
        let _ = printf "F%d %s  " (floorNum + 1) (if elevatorFloor = floorNum then "E" else ".") in
        let _ = List.iteri printSlot ORDERED_SLOTS in
        printfn ""
    in
    for floorNum = ((Array.length floors) - 1) downto 0 do printFloor floorNum
;;

let rec loop floors elevatorFloor r c =
    let moveCursor r c key =
        match key with
        | ConsoleKey.RightArrow -> (r, c + 1)
        | ConsoleKey.LeftArrow -> (r, c - 1)
        | ConsoleKey.UpArrow -> (r - 1, c)
        | ConsoleKey.DownArrow -> (r + 1, c)
        | _ -> (r, c)
    in
    let _ = drawGridWithCursor floors elevatorFloor r c in
    let keyInfo = Console.ReadKey(true) in
    let (newR, newC) = moveCursor r c keyInfo.Key in
    loop floors elevatorFloor newR newC
;;

Console.CursorVisible <- false;
Console.Clear();

let initialFloors =
    [|
        Set.ofList [ Gen P; Mic P ];
        Set.ofList [ Gen C; Gen B; Gen R; Gen F ];
        Set.ofList [ Mic C; Mic B; Mic R; Mic F ];
        Set.empty<Slot>
    |]
in
loop initialFloors 0 0 0
;;
