open System;;
open System.Security.Cryptography;;

exception Ex of string;;

let toByteArray str =
    Seq.toArray (Seq.map (fun (elem : char) -> Convert.ToByte(elem)) str)
;;

let BYTE_TO_STRING = [| for i in 0 .. 0xff -> sprintf "%02x" i |];;
let BYTE_TO_BYTE_STRING = Array.map toByteArray BYTE_TO_STRING;;

let toHexString byteArray =
    Array.fold (fun state (elem : byte) -> state + BYTE_TO_STRING.[int elem]) "" byteArray
;;

type Key = { index : uint64; nibble : byte; bytes : byte[] };;

let g_md5 = MD5.Create("MD5");;

let byteArrayToStringByteArray bytes =
    let output = Array.create ((Array.length bytes) * 2) 0uy in
    Array.iteri (fun i b ->
        let nibbles = BYTE_TO_BYTE_STRING.[int b] in
        let _ = output.[i * 2] <- nibbles.[0] in
        output.[(i * 2) + 1] <- nibbles.[1]
    ) bytes;
    (*printfn "%s -> %s" (toHexString bytes) (toHexString output)*)
    output
;;

let rec generateStretchedHash stretchSize (bytes:byte[]) =
    let md5Bytes = g_md5.ComputeHash(bytes) in
    if stretchSize = 0 then
        md5Bytes
    else
        generateStretchedHash (stretchSize - 1) (byteArrayToStringByteArray md5Bytes)
;;

let findFirstNibbleSequence numConsecutive nibble bytes =
    let addConsecutiveNibble (startingPoint, runSoFar, currentNibbleIndex) b =
        (*let _ = printfn "%02x (%01x %01x) - start %d, run %d, idx %d" b ((b >>> 4) &&& 0xfuy) (b &&& 0xfuy) startingPoint runSoFar currentNibbleIndex in*)
        let nextNibbleIndex = currentNibbleIndex + 2 in
        let highNibbleMatches = ((b >>> 4) &&& 0xfuy) = nibble in
        let lowNibbleMatches = (b &&& 0xfuy) = nibble in
        if runSoFar < numConsecutive then
            let (startingPoint1, runSoFar1) =
                if highNibbleMatches then
                    if runSoFar = 0 then
                        (currentNibbleIndex, runSoFar + 1)
                    else
                        (startingPoint, runSoFar + 1)
                else
                    (0, 0)
            in
            if runSoFar1 >= numConsecutive then
                (startingPoint1, runSoFar1, nextNibbleIndex)
            elif lowNibbleMatches then
                if runSoFar1 = 0 then
                    (currentNibbleIndex + 1, runSoFar1 + 1, nextNibbleIndex)
                else
                    (startingPoint1, runSoFar1 + 1, nextNibbleIndex)
            else
                (0, 0, nextNibbleIndex)
        else
            let _ = assert(runSoFar = numConsecutive) in
            (startingPoint, runSoFar, nextNibbleIndex)
    in
    let (runStart, runLength, _) = Array.fold addConsecutiveNibble (0, 0, 0) bytes in
    (runStart, runLength)
;;

let CANDIDATE_SEQUENCE_LENGTH = 3;;
let CONFIRM_SEQUENCE_LENGTH = 5;;
let CONFIRM_INDEX_RANGE = 1000UL;;

let createCandidateKey index bytes =
    let findEarliestNibbleSequence (earliestSequenceStart, earliestSequenceNibble) nibble =
        let (runStart, runLength) = findFirstNibbleSequence CANDIDATE_SEQUENCE_LENGTH nibble bytes in
        if runLength = CANDIDATE_SEQUENCE_LENGTH then
            if earliestSequenceStart = -1 then
                (*let _ = printfn "found first candidate sequence at %d, nibble %1x" runStart nibble in*)
                (runStart, nibble)
            elif runStart < earliestSequenceStart then
                (*let _ = printfn "found new first candidate sequence at %d, nibble %1x" runStart nibble in*)
                (runStart, nibble)
            else
                let _ = assert(runStart <> earliestSequenceStart) in
                (earliestSequenceStart, earliestSequenceNibble)
        else
            (earliestSequenceStart, earliestSequenceNibble)
    in
    let (earliestSequenceStart, earliestSequenceNibble) = Seq.fold findEarliestNibbleSequence (-1, 0uy) (seq { 0uy .. 0xfuy }) in
    if earliestSequenceStart <> -1 then
        (*let _ = printfn "using candidate with sequence at %d, nibble %1x" earliestSequenceStart earliestSequenceNibble in*)
        Some { index = index; nibble = earliestSequenceNibble; bytes = bytes }
    else
        (*let _ = printfn "no sequence found" in*)
        None
;;

let generateKeys salt numKeysToGenerate stretchSize =
    let rec helper keysSoFar keysSoFarLength candidateKeys index =
        let _ =
            if (index % 1000UL) = 0UL then
                printfn "index %u, keys so far %d, num candidates %d" index keysSoFarLength (List.length candidateKeys)
            else
                ()
        in
        let md5Bytes = generateStretchedHash stretchSize (toByteArray (sprintf "%s%u" salt index)) in
        let md5BytesString = toHexString md5Bytes in
        (*let _ = printfn "index %u, hash %s, num candidates %d" index md5BytesString (List.length candidateKeys) in*)
        let tryConfirmCandidateKey (newKeysSoFar, newKeysSoFarLength, newCandidateKeys) candidateKey =
            if index <= candidateKey.index + CONFIRM_INDEX_RANGE then
                let (_, seqLength) = findFirstNibbleSequence CONFIRM_SEQUENCE_LENGTH candidateKey.nibble md5Bytes in
                if seqLength = CONFIRM_SEQUENCE_LENGTH then
                    (*let _ = printfn "confirming candidate index %u - %s with %s at index %u" candidateKey.index (toHexString candidateKey.bytes) md5BytesString index in*)
                    (candidateKey :: newKeysSoFar, newKeysSoFarLength + 1, newCandidateKeys)
                else
                    (newKeysSoFar, newKeysSoFarLength, candidateKey :: newCandidateKeys)
            else
                (*let _ = printfn "at index %u, aging out candidate index %u" index candidateKey.index in*)
                (newKeysSoFar, newKeysSoFarLength, newCandidateKeys)
        in
        let (confirmedKeysSoFar, confirmedKeysSoFarLength, remainingCandidateKeys) = List.fold tryConfirmCandidateKey (keysSoFar, keysSoFarLength, []) candidateKeys in
        if confirmedKeysSoFarLength < numKeysToGenerate then
            let newCandidateKeys =
                match createCandidateKey index md5Bytes with
                | Some candidateKey -> candidateKey :: remainingCandidateKeys
                | None -> remainingCandidateKeys
            in
            helper confirmedKeysSoFar confirmedKeysSoFarLength newCandidateKeys (index + 1UL)
        elif not (List.isEmpty remainingCandidateKeys) then
            helper confirmedKeysSoFar confirmedKeysSoFarLength remainingCandidateKeys (index + 1UL)
        else
            confirmedKeysSoFar
    in
    helper [] 0 [] 0UL
;;

[<EntryPoint>]
let main argv =
    match argv with
    | [|salt; numKeysStr; stretchSizeStr|] ->
        let numKeys = Convert.ToInt32(numKeysStr) in
        let stretchSize = Convert.ToInt32(stretchSizeStr) in
        let keys = generateKeys salt numKeys stretchSize in
        let sortedKeys = List.sortBy (fun elem -> elem.index) keys in
        let _ = List.mapi (fun i elem -> printfn "key %2d - index: %7u, %s" (i + 1) elem.index (toHexString elem.bytes)) sortedKeys in
        ()
    | _ -> printfn "need salt, num keys, stretch size str"
    0
;;
