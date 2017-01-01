open System;;
open System.Text.RegularExpressions;;

exception Ex of string;;

type Instruction =
    | CopyImmediate of int * string
    | CopyRegister of string * string
    | Inc of string
    | Dec of string
    | JumpNotZero of string * int
;;

type State = { inst : int; registers : Map<string, int> };;

let rexmatch pat str =
    let m = Regex.Match(str, pat) in
    if m.Groups.Count = 0 then
        []
    else
        List.tail [ for g in m.Groups -> g.Value ]
;;

let parseInstruction line =
    match rexmatch @"^cpy (-?\d+) ([a-zA-Z]+)$" line with
    | [a; b] -> CopyImmediate(Convert.ToInt32(a), b)
    | [] ->
        match rexmatch @"^cpy ([a-zA-Z]+) ([a-zA-Z]+)$" line with
        | [a; b] -> CopyRegister(a, b)
        | [] ->
            match rexmatch @"^inc ([a-zA-Z]+)$" line with
            | [a] -> Inc(a)
            | [] ->
                match rexmatch @"^dec ([a-zA-Z]+)$" line with
                | [a] -> Dec(a)
                | [] ->
                    match rexmatch @"^jnz ([a-zA-Z]+) (-?\d+)$" line with
                    | [a; b] -> JumpNotZero(a, Convert.ToInt32(b))
                    | _ -> raise (Ex (sprintf "invalid directive! %s" line))
                | _ -> raise (Ex (sprintf "invalid directive! %s" line))
            | _ -> raise (Ex (sprintf "invalid directive! %s" line))
        | _ -> raise (Ex (sprintf "invalid directive! %s" line))
    | _ -> raise (Ex (sprintf "invalid directive! %s" line))
;;

let rec processLines programSoFar =
    let line = Console.ReadLine() in
    if String.length line = 0 then
        programSoFar
    else
        let inst = parseInstruction line in
        processLines (programSoFar @ [inst])
;;

let rec runProgram program state =
    state
;;

let REGISTERS =
    [
        "a";
        "b";
        "c";
        "d";
    ]
;;

let instructionList = processLines [] in
let program = Array.ofList instructionList in
let _ = List.iter (printfn "%A") instructionList in
let initialRegisters = List.fold (fun map reg -> Map.add reg 0 map) Map.empty REGISTERS in
let finalState = runProgram program { inst = 0; registers = initialRegisters } in
printfn "%A" finalState
;;
