open System;;

let countBits value =
    let incIfBit sofar bitPos =
        if (value &&& (1UL <<< bitPos)) = 0UL then
            sofar
        else
            sofar + 1
    in
    Seq.fold incIfBit 0 (seq { 0 .. 63 })
;;

let isOpen favNumber (x, y) =
    let ui64X = uint64 x in
    let ui64Y = uint64 y in
    let calc = (ui64X * ui64X) + (3UL * ui64X) + (2UL * ui64X * ui64Y) + ui64Y + (ui64Y * ui64Y) + (uint64 favNumber) in
    ((countBits calc) % 2) = 0
;;

let printGrid favNumber sizeX sizeY =
    let printRow y =
        printf "%-2d " y;
        for x = 0 to (sizeX - 1) do printf "%2s " (if (isOpen favNumber (x, y)) then "." else "#");
        printfn ""
    in
    printf "   ";
    for x = 0 to (sizeX - 1) do printf "%2d " x;
    printfn "";
    for y = 0 to (sizeY - 1) do printRow y;
    printfn "";
;;

[<EntryPoint>]
let main argv =
    match argv with
    | [|favoriteNumberStr; destXStr; destYStr|] ->
        let favoriteNumber = Convert.ToInt32(favoriteNumberStr) in
        let destX = Convert.ToInt32(destXStr) in
        let destY = Convert.ToInt32(destYStr) in
        let _ = printGrid favoriteNumber 10 10 in
        printfn "%d %d %d" favoriteNumber destX destY
    | _ -> printfn "need favorite number, dest x, and dest y"
    0
;;

