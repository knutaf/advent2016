open System;;
open System.Security.Cryptography;;

exception Ex of string;;

let toByteArray str =
    Seq.toArray (Seq.map (fun (elem : char) -> Convert.ToByte(elem)) str)
;;

let BYTE_TO_STRING =
    [|
        "00";
        "01";
        "02";
        "03";
        "04";
        "05";
        "06";
        "07";
        "08";
        "09";
        "0a";
        "0b";
        "0c";
        "0d";
        "0e";
        "0f";
        "10";
        "11";
        "12";
        "13";
        "14";
        "15";
        "16";
        "17";
        "18";
        "19";
        "1a";
        "1b";
        "1c";
        "1d";
        "1e";
        "1f";
        "20";
        "21";
        "22";
        "23";
        "24";
        "25";
        "26";
        "27";
        "28";
        "29";
        "2a";
        "2b";
        "2c";
        "2d";
        "2e";
        "2f";
        "30";
        "31";
        "32";
        "33";
        "34";
        "35";
        "36";
        "37";
        "38";
        "39";
        "3a";
        "3b";
        "3c";
        "3d";
        "3e";
        "3f";
        "40";
        "41";
        "42";
        "43";
        "44";
        "45";
        "46";
        "47";
        "48";
        "49";
        "4a";
        "4b";
        "4c";
        "4d";
        "4e";
        "4f";
        "50";
        "51";
        "52";
        "53";
        "54";
        "55";
        "56";
        "57";
        "58";
        "59";
        "5a";
        "5b";
        "5c";
        "5d";
        "5e";
        "5f";
        "60";
        "61";
        "62";
        "63";
        "64";
        "65";
        "66";
        "67";
        "68";
        "69";
        "6a";
        "6b";
        "6c";
        "6d";
        "6e";
        "6f";
        "70";
        "71";
        "72";
        "73";
        "74";
        "75";
        "76";
        "77";
        "78";
        "79";
        "7a";
        "7b";
        "7c";
        "7d";
        "7e";
        "7f";
        "80";
        "81";
        "82";
        "83";
        "84";
        "85";
        "86";
        "87";
        "88";
        "89";
        "8a";
        "8b";
        "8c";
        "8d";
        "8e";
        "8f";
        "90";
        "91";
        "92";
        "93";
        "94";
        "95";
        "96";
        "97";
        "98";
        "99";
        "9a";
        "9b";
        "9c";
        "9d";
        "9e";
        "9f";
        "a0";
        "a1";
        "a2";
        "a3";
        "a4";
        "a5";
        "a6";
        "a7";
        "a8";
        "a9";
        "aa";
        "ab";
        "ac";
        "ad";
        "ae";
        "af";
        "b0";
        "b1";
        "b2";
        "b3";
        "b4";
        "b5";
        "b6";
        "b7";
        "b8";
        "b9";
        "ba";
        "bb";
        "bc";
        "bd";
        "be";
        "bf";
        "c0";
        "c1";
        "c2";
        "c3";
        "c4";
        "c5";
        "c6";
        "c7";
        "c8";
        "c9";
        "ca";
        "cb";
        "cc";
        "cd";
        "ce";
        "cf";
        "d0";
        "d1";
        "d2";
        "d3";
        "d4";
        "d5";
        "d6";
        "d7";
        "d8";
        "d9";
        "da";
        "db";
        "dc";
        "dd";
        "de";
        "df";
        "e0";
        "e1";
        "e2";
        "e3";
        "e4";
        "e5";
        "e6";
        "e7";
        "e8";
        "e9";
        "ea";
        "eb";
        "ec";
        "ed";
        "ee";
        "ef";
        "f0";
        "f1";
        "f2";
        "f3";
        "f4";
        "f5";
        "f6";
        "f7";
        "f8";
        "f9";
        "fa";
        "fb";
        "fc";
        "fd";
        "fe";
        "ff";
    |]
;;

let toHexString byteArray =
    Array.fold (fun state (elem : byte) -> state + BYTE_TO_STRING.[int elem]) "" byteArray
;;

let BYTE_TO_BYTE_STRING = Array.map toByteArray BYTE_TO_STRING;;

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
        (*
        let _ =
            if (index % 1000UL) = 0UL then
                printfn "index %u, keys so far %d, num candidates %d" index keysSoFarLength (List.length candidateKeys)
            else
                ()
        in
        *)
        let md5Bytes = generateStretchedHash stretchSize (toByteArray (sprintf "%s%u" salt index)) in
        let md5BytesString = toHexString md5Bytes in
        (*let _ = printfn "index %u, hash %s, num candidates %d" index md5BytesString (List.length candidateKeys) in*)
        let tryConfirmCandidateKey (newKeysSoFar, newKeysSoFarLength, newCandidateKeys) candidateKey =
            if index <= candidateKey.index + CONFIRM_INDEX_RANGE then
                let (_, seqLength) = findFirstNibbleSequence CONFIRM_SEQUENCE_LENGTH candidateKey.nibble md5Bytes in
                if seqLength = CONFIRM_SEQUENCE_LENGTH then
                    if not (List.exists (fun elem -> elem.index = candidateKey.index) newKeysSoFar) then
                        (*let _ = printfn "confirming candidate index %u - %s with %s at index %u" candidateKey.index (toHexString candidateKey.bytes) md5BytesString index in*)
                        (candidateKey :: newKeysSoFar, newKeysSoFarLength + 1, newCandidateKeys)
                    else
                        (*let _ = printfn "at index %u, discarding duplicate candidate index %u" index candidateKey.index in*)
                        (newKeysSoFar, newKeysSoFarLength, newCandidateKeys)
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
