open System;;
open System.Text.RegularExpressions;;

exception Ex of string;;

let generateNextLine prevLine =
    prevLine
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
