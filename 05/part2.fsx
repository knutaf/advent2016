open System;;
open System.Security.Cryptography;;

exception Ex of string;;

let g_md5 = MD5.Create();;

let toByteArray str =
    Seq.toArray (Seq.map (fun (elem : char) -> Convert.ToByte(elem)) str)
;;

let getInterestingPosAndLetter (byteArray : byte[]) =
    match byteArray.[..3] with
    | [| 0uy; 0uy; x; y |] ->
        let highNibbleX = x &&& 0xf0uy in
        let lowNibbleX = x &&& 0x0fuy in
        if highNibbleX = 0uy then
            let highNibbleY = y &&& 0xf0uy in
            Some (Convert.ToInt32(lowNibbleX), Convert.ToString(highNibbleY, 16).[0])
        else
            None
    | _ -> None
;;


let doorId = Console.ReadLine() in
let rec findNextInterestingLetter (index : uint64) =
    if (index % 100000UL) = 0UL then printfn "index %d" index;
    let str = doorId + Convert.ToString(index) in
    let hsh = g_md5.ComputeHash(toByteArray str) in
    match getInterestingPosAndLetter hsh with
    | None -> findNextInterestingLetter (index + 1UL)
    | Some (pos, letter) -> (pos, letter, index)
in
let password = [| '_'; '_'; '_'; '_'; '_'; '_'; '_'; '_'; |] in
let rec setPasswordLetters index lettersLeft =
    if lettersLeft = 0 then
        ()
    else
        let (pos, letter, nextIndex) = findNextInterestingLetter index in
        let _ = printfn "pos: %d, letter: %c, index: %u" pos letter nextIndex in
        let nextLettersLeft =
            if pos < (Array.length password) then
                if password.[pos] = '_' then
                    let _ = Array.set password pos letter in
                    lettersLeft - 1
                else
                    lettersLeft
            else
                lettersLeft
        in
        setPasswordLetters (nextIndex + 1UL) nextLettersLeft
in
let _ = setPasswordLetters 0UL 8 in
let passwordStr = Array.fold (fun sofar (elem : char) -> sofar + Convert.ToString(elem)) "" password in
printfn "password: %s (%A)" passwordStr password
;;
