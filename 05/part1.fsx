open System;;
open System.Security.Cryptography;;

exception Ex of string;;

let measureTime fn =
    let stopWatch = System.Diagnostics.Stopwatch.StartNew();
    fn ();
    stopWatch.Stop();
    printfn "%f ms elapsed" stopWatch.Elapsed.TotalMilliseconds;
    stopWatch.Elapsed.TotalMilliseconds
;;

let g_md5 = MD5.Create("MD5");;

let toByteArray str =
    Seq.toArray (Seq.map (fun (elem : char) -> Convert.ToByte(elem)) str)
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
