open System;;
open System.Security.Cryptography;;

exception Ex of string;;

let g_md5 = MD5.Create("MD5");;

let toByteArray str =
    Seq.toArray (Seq.map (fun (elem : char) -> Convert.ToByte(elem)) str)
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

let getInterestingDigit (byteArray : byte[]) =
    match byteArray.[..2] with
    | [| 0uy; 0uy; x |] ->
        let highNibble = x &&& 0xf0uy in
        let lowNibble = x &&& 0x0fuy in
        if highNibble = 0uy then
            Some (Convert.ToString(lowNibble, 16))
        else
            None
    | _ -> None
;;

let doorId = Console.ReadLine() in
let rec findNextInterestingLetter (index : uint64) =
    if (index % 100000UL) = 0UL then printfn "index %d" index;
    let str = doorId + Convert.ToString(index) in
    let hsh = g_md5.ComputeHash(toByteArray str) in
    match getInterestingDigit hsh with
    | None -> findNextInterestingLetter (index + 1UL)
    | Some letter -> (letter, index)
in
let rec generatePasswordLetters sofar index lettersLeft =
    if lettersLeft = 0 then
        sofar
    else
        let (letter, nextIndex) = findNextInterestingLetter index in
        let _ = printfn "letter: %s at %u" letter nextIndex in
        generatePasswordLetters (sofar + letter) (nextIndex + 1UL) (lettersLeft - 1)
in
measureTime (fun () -> printfn "password: %s" (generatePasswordLetters "" 0UL 8))
;;
