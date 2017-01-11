open System;;
open System.Security.Cryptography;;

exception Ex of string;;

let toByteArray str =
    Seq.toArray (Seq.map (fun (elem : char) -> Convert.ToByte(elem)) str)
;;

let BYTE_TO_STRING = [| for i in 0 .. 0xff -> sprintf "%02x" i |];;
let toHexString byteArray =
    Array.fold (fun state (elem : byte) -> state + BYTE_TO_STRING.[int elem]) "" byteArray
;;

let g_md5 = MD5.Create("MD5");;

let GRID_SIZE_X = 4;;
let GRID_SIZE_Y = 4;;

type Direction =
    | Up
    | Down
    | Left
    | Right
;;

let getOpenDoors x y pathStr =
    let isHashNibbleOpen nibble =
        nibble > 0xauy
    in
    let md5Bytes = g_md5.ComputeHash(toByteArray pathStr) in
    let _ = printfn "hash: %s" (toHexString md5Bytes) in
    let ls0 = [] in
    let ls1 =
        if y > 0 then
            if isHashNibbleOpen ((md5Bytes.[0] &&& 0xf0uy) >>> 4) then
                Up :: ls0
            else
                ls0
        else
            ls0
    in
    let ls2 =
        if y < GRID_SIZE_Y - 1 then
            if isHashNibbleOpen (md5Bytes.[0] &&& 0x0fuy) then
                Down :: ls1
            else
                ls1
        else
            ls1
    in
    let ls3 =
        if x > 0 then
            if isHashNibbleOpen ((md5Bytes.[1] &&& 0xf0uy) >>> 4) then
                Left :: ls2
            else
                ls2
        else
            ls2
    in
    let ls4 =
        if x < GRID_SIZE_X - 1 then
            if isHashNibbleOpen (md5Bytes.[1] &&& 0x0fuy) then
                Right :: ls3
            else
                ls3
        else
            ls3
    in
    ls4
;;

[<EntryPoint>]
let main argv =
    match argv with
    | [| passcode; path |] ->
        printfn "dirs: %A" (getOpenDoors 2 2 (passcode + path))
    | _ -> printfn "need passcode"
    0
;;
