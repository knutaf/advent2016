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
let START = (0, 0);;
let TARGET = (3, 3);;

type Direction =
    | Up
    | Down
    | Left
    | Right
;;

let getOpenDoors (x, y) pathStr =
    let isHashNibbleOpen nibble =
        nibble > 0xauy
    in
    let md5Bytes = g_md5.ComputeHash(toByteArray pathStr) in
    //let _ = printfn "hash: %s" (toHexString md5Bytes) in
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
        if x > 0 then
            if isHashNibbleOpen ((md5Bytes.[1] &&& 0xf0uy) >>> 4) then
                Left :: ls1
            else
                ls1
        else
            ls1
    in
    let ls3 =
        if y < GRID_SIZE_Y - 1 then
            if isHashNibbleOpen (md5Bytes.[0] &&& 0x0fuy) then
                Down :: ls2
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

let movePos (x, y) dir =
    match dir with
    | Up -> (x, y - 1)
    | Down -> (x, y + 1)
    | Left -> (x - 1, y)
    | Right -> (x + 1, y)
;;

let stringFromDir dir =
    match dir with
    | Up -> "U"
    | Down -> "D"
    | Left -> "L"
    | Right -> "R"
;;

let dirFromChar ch =
    match ch with
    | 'U' -> Up
    | 'D' -> Down
    | 'L' -> Left
    | 'R' -> Right
    | _ -> raise (Ex (sprintf "invalid dir char! %c" ch))
;;

let verifyPath pathStr =
    let endPos =
        Seq.fold (fun pos ch ->
            let nextPos = movePos pos (dirFromChar ch) in
            let _ = printfn "%A -> %c -> %A" pos ch nextPos in
            nextPos
            ) START pathStr
    in
    printfn "ended at: %A" endPos
;;

// TODO: find the shortest path, not the first successful path
let dfs passcode =
    let rec helper pos pathStr =
        if pos = TARGET then
            Some pathStr
        else
            let openDoors = getOpenDoors pos pathStr in
            List.fold (fun anyPath dir ->
                if anyPath.IsSome then
                    anyPath
                else
                    helper (movePos pos dir) (pathStr + (stringFromDir dir))
                ) None openDoors
    in
    helper START passcode
;;

[<EntryPoint>]
let main argv =
    match argv with
    | [| passcode |] ->
        let pathWithPasscode = (dfs passcode).Value in
        let path = pathWithPasscode.Substring(String.length passcode) in
        let _ = verifyPath path in
        printfn "path: %s" path
    | _ -> printfn "need passcode"
    0
;;
