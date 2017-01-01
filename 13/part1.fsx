open System;;

[<EntryPoint>]
let main argv =
    match argv with
    | [|favoriteNumberStr; destXStr; destYStr|] ->
        let favoriteNumber = Convert.ToInt32(favoriteNumberStr) in
        let destX = Convert.ToInt32(destXStr) in
        let destY = Convert.ToInt32(destYStr) in
        printfn "%d %d %d" favoriteNumber destX destY
    | _ -> printfn "need favorite number, dest x, and dest y"
    0
;;

