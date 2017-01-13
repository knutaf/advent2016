open System;;
open System.Text.RegularExpressions;;

exception Ex of string;;

type Elf = { id: int; presents: int };;

let stealUntilDone startingNumElves =
    let runRound elves numElves =
        let _ = assert(numElves > 1) in
        let isOddNumber = (numElves % 2) = 1 in
        let elvesStartingRound =
            if isOddNumber then
                List.tail elves
            else
                elves
        in
        let numPresentsForEach = (startingNumElves / (numElves / 2)) in
        List.foldBack (fun elf (nextElvesSoFar, nextElvesSoFarLength, numTraversed) ->
            if (not isOddNumber) && (numTraversed = 0) then
                (nextElvesSoFar, nextElvesSoFarLength, numTraversed + 1)
            else
                if ((numTraversed % 2) = 0) = isOddNumber then
                    let newElf =
                        {
                            id = elf.id;
                            presents =
                                numPresentsForEach +
                                if isOddNumber && (numTraversed = 0) then
                                    1
                                else
                                    0
                        }
                    in
                    (newElf :: nextElvesSoFar, nextElvesSoFarLength + 1, numTraversed + 1)
                else
                    (nextElvesSoFar, nextElvesSoFarLength, numTraversed + 1)
            ) elvesStartingRound ([], 0, 0)
    in
    let initialElves = List.ofSeq (seq { for id in 1 .. startingNumElves -> { id = id; presents = 1 } }) in
    let (nextElves, nextElvesLength, numTraversed) = runRound initialElves startingNumElves in
    let _ = printfn "after round: %A" (List.map (fun elf -> (elf.id, elf.presents)) nextElves) in
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
