open System;;
open System.Text.RegularExpressions;;

exception Ex of string;;

type Node = { data : bool; mutable next : Node option };;

let takeFirstN n ls =
    List.fold (fun sofar elem ->
        if List.length sofar < n then
            sofar @ [elem]
        else
            sofar
        ) [] ls
;;

let parseInput dataStr =
    let convertAndAppend (listEnd, listLengthSoFar) ch =
        let _ = assert(listEnd.next.IsNone) in
        let d =
            match ch with
            | '0' -> false
            | '1' -> true
            | _ -> raise (Ex (sprintf "invalid char! %c" ch))
        in
        let _ = listEnd.next <- Some { data = d; next = None } in
        (listEnd.next.Value, listLengthSoFar + 1)
    in
    let dummyHead = { data = false; next = None } in
    let (listEnd, listLength) = Seq.fold convertAndAppend (dummyHead, 0) dataStr in
    let _ = assert(listEnd.next.IsNone) in
    let _ = assert(listLength > 0) in
    match dummyHead.next with
    | None -> raise (Ex (sprintf "invalid string!? %A" dataStr))
    | Some head -> (head, listLength)
;;

let dataToString ls =
    let rec processNext sofar ls =
        let newSoFar = sofar + (if ls.data then "1" else "0") in
        match ls.next with
        | None -> newSoFar
        | Some nextNode -> processNext newSoFar nextNode
    in
    processNext "" ls
;;

let munge a aLength =
    let rec processNext bSoFar bLength a =
        let newBFront = { data = (not a.data); next = bSoFar } in
        let newBLength = bLength + 1 in
        match a.next with
        | None -> (newBFront, newBLength, a)
        | Some nextNode -> processNext (Some newBFront) newBLength nextNode
    in
    let (bInverse, bInverseLength, aEnd) = processNext None 0 a
    let bWithSeparator = { data = false; next = Some bInverse } in
    let _ = assert(aEnd.next.IsNone) in
    let _ = aEnd.next <- Some bWithSeparator in
    (a, aLength + 1 + bInverseLength)
;;

(*
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
*)

[<EntryPoint>]
let main argv =
    match argv with
    | [| dataStr; diskSizeStr |] ->
        let (inputData, inputDataLength) = parseInput dataStr in
        let diskSize = Convert.ToInt32(diskSizeStr) in
        let _ = printfn "input data: %s (%d), size: %d" (dataToString inputData) inputDataLength diskSize in
        if (diskSize % 2) = 0 then
            (*
            printfn "munge 1: %s" (dataToString (fst (munge inputData (List.length inputData))))
            let (mungedData, mungedDataLength) = mungeUntilSize inputData (List.length inputData) diskSize in
            let _ = printfn "munged: %d" mungedDataLength in
            printfn "checksum: %s" (dataToString (calculateChecksum mungedData))
            *)
            ()
        else
            printfn "can't calculate checksum of odd size!"
    | _ -> printfn "need data string and disk size"
    0
;;
