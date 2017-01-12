open System;;
open System.Text.RegularExpressions;;

exception Ex of string;;

let countSafeTiles str =
    Seq.fold (fun numSafeTilesSoFar tile ->
        match tile with
        | '^' -> numSafeTilesSoFar
        | '.' -> (numSafeTilesSoFar + 1)
        | _ -> raise (Ex "invalid char!")
        ) 0 str
;;

let generateNextLine (prevLine:string) =
    let generateTile i tile =
        let leftTile =
            if i = 0 then
                '.'
            else
                prevLine.[i - 1]
        in
        let centerTile = tile in
        let rightTile =
            if i = (String.length prevLine) - 1 then
                '.'
            else
                prevLine.[i + 1]
        in
        if (leftTile = '^' && centerTile = '^' && rightTile = '.') ||
           (leftTile = '.' && centerTile = '^' && rightTile = '^') ||
           (leftTile = '^' && centerTile = '.' && rightTile = '.') ||
           (leftTile = '.' && centerTile = '.' && rightTile = '^') then
            '^'
        else
            '.'
    in
    String.mapi generateTile prevLine
;;

[<EntryPoint>]
let main argv =
    match argv with
    | [| numLinesToGenerateStr |] ->
        let numLinesToGenerate = Convert.ToInt32(numLinesToGenerateStr) in
        let firstLine = Console.ReadLine() in
        let numSafeTiles = countSafeTiles firstLine in
        //let _ = printfn "%s (%d)" firstLine numSafeTiles in
        let (lastLine, totalSafeTiles) =
            Seq.fold (fun (prevLine, numSafeTilesSoFar) _ ->
                let nextLine = generateNextLine prevLine in
                let safeTilesInNextLine = countSafeTiles nextLine in
                //let _ = printfn "%s (%d)" nextLine safeTilesInNextLine in
                (nextLine, numSafeTilesSoFar + safeTilesInNextLine)
                ) (firstLine, numSafeTiles) (seq { 1 .. (numLinesToGenerate - 1) })
        in
        printfn "total safe tiles: %d" totalSafeTiles
    | _ -> printfn "need num lines to generate"
    0
;;
