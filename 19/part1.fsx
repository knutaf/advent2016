open System;;
open System.Text.RegularExpressions;;

exception Ex of string;;

type Elf = { id: int; presents: int };;

let stealUntilDone numElves =
    let initialElves = List.ofSeq (seq { for id in 1 .. numElves -> { id = id; presents = 1 } }) in
    let _ = printfn "len: %d" (List.length initialElves) in
    List.head initialElves
;;

[<EntryPoint>]
let main argv =
    match argv with
    | [| numElvesStr |] ->
        let numElves = Convert.ToInt32(numElvesStr) in
        let whichElf = stealUntilDone numElves in
        printfn "elf %d has it" whichElf.id
    | _ -> printfn "need num elves"
    0
;;
