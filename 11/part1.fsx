open System;;

let drawGridWithCursor r c =
    Console.SetCursorPosition(0, 0);
    printfn "+----+----+----+";
    printfn "| %s  | %s  | %s  |" (if r = 0 && c = 0 then "X" else " ") (if r = 0 && c = 1 then "X" else " ") (if r = 0 && c = 2 then "X" else " ");
    printfn "|----|----|----|";
    printfn "| %s  | %s  | %s  |" (if r = 1 && c = 0 then "X" else " ") (if r = 1 && c = 1 then "X" else " ") (if r = 1 && c = 2 then "X" else " ");
    printfn "+----+----+----+";
;;

let rec loop r c =
    let moveCursor r c key =
        match key with
        | ConsoleKey.RightArrow -> (r, c + 1)
        | ConsoleKey.LeftArrow -> (r, c - 1)
        | ConsoleKey.UpArrow -> (r - 1, c)
        | ConsoleKey.DownArrow -> (r + 1, c)
        | _ -> (r, c)
    in
    let _ = drawGridWithCursor r c in
    let keyInfo = Console.ReadKey(true) in
    let (newR, newC) = moveCursor r c keyInfo.Key in
    loop newR newC
in
Console.CursorVisible <- false;
Console.Clear();
loop 0 0
;;
