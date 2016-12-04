open System;;
open System.Text.RegularExpressions;;

exception Ex of string;;

let rexmatch pat str =
    let m = Regex.Match(str, pat) in
    if m.Groups.Count = 0 then
        []
    else
        List.tail [ for g in m.Groups -> g.Value ]
;;

let parseTriangleParts line =
    let parts = rexmatch "^\s*(\d+)\s*(\d+)\s*(\d+)\s*$" line in
    match parts with
    | side1 :: side2 :: side3 :: [] -> (Convert.ToInt32(side1), Convert.ToInt32(side2), Convert.ToInt32(side3))
    | _ -> raise (Ex (sprintf "invalid triangle! %s" line))
;;

let isValidTriangle (s1, s2, s3) =
    ((s1 + s2) > s3) &&
    ((s2 + s3) > s1) &&
    ((s1 + s3) > s2)
;;

let rec processLines numValid =
    let line1 = Console.ReadLine() in
    if String.length line1 = 0 then
        numValid
    else
        let line2 = Console.ReadLine() in
        let line3 = Console.ReadLine() in
        let (t1s1, t2s1, t3s1) = parseTriangleParts line1 in
        let (t1s2, t2s2, t3s2) = parseTriangleParts line2 in
        let (t1s3, t2s3, t3s3) = parseTriangleParts line3 in
        let tris =
            [
                (t1s1, t1s2, t1s3);
                (t2s1, t2s2, t2s3);
                (t3s1, t3s2, t3s3);
            ]
        in
        let newNumValid = List.fold (fun numValid elem -> if isValidTriangle elem then numValid + 1 else numValid) numValid tris in
        processLines newNumValid
;;

printfn "%d valid triangles" (processLines 0)
