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
    true
;;

[<EntryPoint>]
let main argv =
    match argv with
    | [|favoriteNumberStr; destXStr; destYStr|] ->
        let favoriteNumber = Convert.ToInt32(favoriteNumberStr) in
        let destX = Convert.ToInt32(destXStr) in
        let destY = Convert.ToInt32(destYStr) in
        let _ = printfn "countbits: %d" (countBits (uint64 favoriteNumber)) in
        printfn "%d %d %d" favoriteNumber destX destY
    | _ -> printfn "need favorite number, dest x, and dest y"
    0
;;

