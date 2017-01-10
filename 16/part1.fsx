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


let munge a =
    let b = List.rev a in
    let bInverse = List.map (not) b in
    a @ (false::bInverse)
;;

let rec mungeUntilSize data size =
    if List.length data >= size then
        data
    else
        mungeUntilSize (munge data) size
;;

[<EntryPoint>]
let main argv =
    match argv with
    | [| dataStr; diskSizeStr |] ->
        let inputData = parseInput dataStr in
        let diskSize = Convert.ToInt32(diskSizeStr) in
        let _ = printfn "input data: %s, size: %d" (dataToString inputData) diskSize in
        printfn "munged: %s" (dataToString (mungeUntilSize inputData diskSize))
    | _ -> printfn "need data string and disk size"
    0
;;
