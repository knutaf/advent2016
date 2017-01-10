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

let munge a aLength =
    let (bInverse, listLength) = List.fold (fun (bSofar, len) elem -> ((not elem) :: bSofar), len + 1) ([], 0) a in
    (a @ (false :: bInverse), (aLength + 1 + listLength))
;;

let rec mungeUntilSize data dataLength size =
    if dataLength > size then
        (takeFirstN size data, size)
    elif dataLength = size then
        (data, dataLength)
    else
        let (newData, newDataLength) = munge data dataLength in
        mungeUntilSize newData newDataLength size
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
            (*
            printfn "munge 1: %s" (dataToString (fst (munge inputData (List.length inputData))))
            *)
            let (mungedData, mungedDataLength) = mungeUntilSize inputData (List.length inputData) diskSize in
            let _ = printfn "munged: %d" mungedDataLength in
            printfn "checksum: %s" (dataToString (calculateChecksum mungedData))
        else
            printfn "can't calculate checksum of odd size!"
    | _ -> printfn "need data string and disk size"
    0
;;
