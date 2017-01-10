open System;;
open System.Text.RegularExpressions;;

exception Ex of string;;

let takeFirstN n ls =
    List.fold (fun sofar elem ->
        if List.length sofar < n then
            sofar @ [elem]
        else
            sofar
        ) [] ls
;;

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
    let dataLength = List.length data in
    if dataLength > size then
        takeFirstN size data
    elif dataLength = size then
        data
    else
        mungeUntilSize (munge data) size
;;

let rec calculateChecksum data =
    let rec helper checksumSoFar data =
        match data with
        | [] -> checksumSoFar
        | a :: b :: rest -> helper (checksumSoFar @ [a = b]) rest
        | _ -> raise (Ex (sprintf "how did this happen!? %A" data))
    in
    let checksumStep = helper [] data in
    if ((List.length checksumStep) % 2 = 1) then
        checksumStep
    else
        calculateChecksum checksumStep
;;

[<EntryPoint>]
let main argv =
    match argv with
    | [| dataStr; diskSizeStr |] ->
        let inputData = parseInput dataStr in
        let diskSize = Convert.ToInt32(diskSizeStr) in
        let _ = printfn "input data: %s, size: %d" (dataToString inputData) diskSize in
        if (diskSize % 2) = 0 then
            let mungedData = mungeUntilSize inputData diskSize in
            let _ = printfn "munged: %s" (dataToString mungedData) in
            printfn "checksum: %s" (dataToString (calculateChecksum mungedData))
        else
            printfn "can't calculate checksum of odd size!"
    | _ -> printfn "need data string and disk size"
    0
;;
