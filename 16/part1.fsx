open System;;
open System.Text.RegularExpressions;;

exception Ex of string;;

let parseInput dataStr =
    List.ofSeq (Seq.map (fun ch ->
        match ch with
        | '0' -> false
        | '1' -> true
        | _ -> raise (Ex (sprintf "invalid char! %c" ch))
    ) dataStr)
;;

let dataToString data =
    List.fold (fun sofar d -> sofar + (if d then "1" else "0")) "" data
;;

[<EntryPoint>]
let main argv =
    match argv with
    | [| dataStr; diskSizeStr |] ->
        let inputData = parseInput dataStr in
        let diskSize = Convert.ToInt32(diskSizeStr) in
        printfn "input data: %s, size: %d" (dataToString inputData) diskSize
    | _ -> printfn "need data string and disk size"
    0
;;
