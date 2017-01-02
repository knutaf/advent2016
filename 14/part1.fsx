open System;;
open System.Security.Cryptography;;

exception Ex of string;;

type Key = { index : uint64; nibble : byte; bytes : byte[] };;

let g_md5 = MD5.Create("MD5");;

let toByteArray str =
    Seq.toArray (Seq.map (fun (elem : char) -> Convert.ToByte(elem)) str)
;;

let toHexString byteArray =
    Array.fold (fun state (elem : byte) -> state + Convert.ToString(elem, 16)) "" byteArray
;;

let containsNibbleSequence numConsecutive nibble bytes =
    let addConsecutiveNibble (maxSequence, numConsecutive) b =
        (*let _ = printfn "%x (%01x %01x)" b ((b >>> 4) &&& 0xfuy) (b &&& 0xfuy) in*)
        let highNibbleMatches = ((b >>> 4) &&& 0xfuy) = nibble in
        let lowNibbleMatches = (b &&& 0xfuy) = nibble in
        let consec1 =
            if highNibbleMatches then
                numConsecutive + 1
            else
                0
        in
        let max1 = max maxSequence consec1 in
        (*let _ = printfn "consec1: %d, max1: %d" consec1 max1 in*)
        let consec2 =
            if lowNibbleMatches then
                consec1 + 1
            else
                0
        in
        (*let _ = printfn "consec2: %d" consec2 in*)
        (max max1 consec2, consec2)
    in
    let (longestSequence, _) = Array.fold addConsecutiveNibble (0, 0) bytes in
    longestSequence >= numConsecutive
;;

let CANDIDATE_SEQUENCE_LENGTH = 3;;
let CONFIRM_SEQUENCE_LENGTH = 5;;
let CONFIRM_INDEX_RANGE = 1000UL;;

let createCandidateKeys index bytes =
    let addKeyIfContainsSequence keysSoFar nibble =
        if containsNibbleSequence CANDIDATE_SEQUENCE_LENGTH nibble bytes then
            (*let _ = printfn "candidate index %u, nibble %1x, %s" index nibble (toHexString bytes) in*)
            { index = index; nibble = nibble; bytes = bytes } :: keysSoFar
        else
            keysSoFar
    in
    Seq.fold addKeyIfContainsSequence [] (seq { 0uy .. 0xfuy })
;;

let generateKeys salt numKeysToGenerate =
    let rec helper keysSoFar keysSoFarLength candidateKeys index =
        let _ =
            if (index % 1000UL) = 0UL then
                printfn "index %u, keys so far %d, num candidates %d" index keysSoFarLength (List.length candidateKeys)
            else
                ()
        in
        let md5Bytes = g_md5.ComputeHash(toByteArray (sprintf "%s%u" salt index)) in
        let tryConfirmCandidateKey (newKeysSoFar, newKeysSoFarLength, newCandidateKeys) candidateKey =
            if newKeysSoFarLength < numKeysToGenerate then
                if index <= candidateKey.index + CONFIRM_INDEX_RANGE then
                    if containsNibbleSequence CONFIRM_SEQUENCE_LENGTH candidateKey.nibble md5Bytes then
                        if not (List.exists (fun elem -> elem.index = candidateKey.index) newKeysSoFar) then
                            let _ = printfn "confirming candidate index %u - %s with %s at index %u" candidateKey.index (toHexString candidateKey.bytes) (toHexString md5Bytes) index in
                            (candidateKey :: newKeysSoFar, newKeysSoFarLength + 1, newCandidateKeys)
                        else
                            let _ = printfn "at index %u, discarding candidate index %u" index candidateKey.index in
                            (newKeysSoFar, newKeysSoFarLength, newCandidateKeys)
                    else
                        (newKeysSoFar, newKeysSoFarLength, candidateKey :: newCandidateKeys)
                else
                    (*let _ = printfn "at index %u, aging out candidate index %u" index candidateKey.index in*)
                    (newKeysSoFar, newKeysSoFarLength, newCandidateKeys)
            else
                let _ = assert (newKeysSoFarLength = numKeysToGenerate) in
                (newKeysSoFar, newKeysSoFarLength, newCandidateKeys)
        in
        let (confirmedKeysSoFar, confirmedKeysSoFarLength, remainingCandidateKeys) = List.fold tryConfirmCandidateKey (keysSoFar, keysSoFarLength, []) candidateKeys in
        if confirmedKeysSoFarLength < numKeysToGenerate then
            let newCandidateKeys = createCandidateKeys index md5Bytes in
            helper confirmedKeysSoFar confirmedKeysSoFarLength (remainingCandidateKeys @ newCandidateKeys) (index + 1UL)
        else
            let _ = assert (confirmedKeysSoFarLength = numKeysToGenerate) in
            confirmedKeysSoFar
    in
    helper [] 0 [] 0UL
;;

[<EntryPoint>]
let main argv =
    match argv with
    | [|salt; numKeysStr|] ->
        let numKeys = Convert.ToInt32(numKeysStr) in
        let keys = generateKeys salt numKeys in
        printfn "last index: %A" (List.fold (fun maxSofar elem -> max maxSofar elem.index) 0UL keys)
    | _ -> printfn "need favorite number, dest x, and dest y"
    0
;;
