open System;;
open System.Text.RegularExpressions;;

exception Ex of string;;

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
        let _ = printfn "%s" firstLine in
        let lastLine =
            Seq.fold (fun prevLine _ ->
                let nextLine = generateNextLine prevLine in
                let _ = printfn "%s" nextLine in
                nextLine
                ) firstLine (seq { 1 .. (numLinesToGenerate - 1) })
        in
        ()
    | _ -> printfn "need num lines to generate"
    0
;;
