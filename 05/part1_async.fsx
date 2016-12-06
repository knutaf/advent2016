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

let NUM_WORKERS = 4UL;;
let NUM_HASHES_PER_WORKER_INSTANCE = 100000UL;;

let toByteArray str =
    Seq.toArray (Seq.map (fun (elem : char) -> Convert.ToByte(elem)) str)
;;

let getInterestingLetter (byteArray : byte[]) =
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

let rec findNextInterestingLetter workerNum (md5 : MD5) doorId sofar (index : uint64) numIndexesToSearch =
    if numIndexesToSearch = 0UL then
        sofar
    else
        (*if numIndexesToSearch = NUM_HASHES_PER_WORKER_INSTANCE then printfn "%u: starting at %u" workerNum index;*)
        if index % 100000UL = 0UL then printfn "%u: index %u" workerNum index;
        let str = doorId + Convert.ToString(index) in
        let hsh = md5.ComputeHash(toByteArray str) in
        let optInterestingLetter = getInterestingLetter hsh in
        let nextSofar =
            match optInterestingLetter with
            | None -> sofar
            | Some s -> printfn "%u: found letter %s at index %u" workerNum s index; (s :: sofar)
        in
        findNextInterestingLetter workerNum md5 doorId nextSofar (index + 1UL) (numIndexesToSearch - 1UL)
in
let createBatchFindNextInterestingLetter doorId startingIndex numHashesPerWorkerInstance =
    seq {
        for i in 0UL .. (NUM_WORKERS - 1UL) do
            yield async {
                let md5 = MD5.Create("MD5") in
                return findNextInterestingLetter i md5 doorId [] (startingIndex + (i * numHashesPerWorkerInstance)) numHashesPerWorkerInstance
            }
    }
;;

let doorId = Console.ReadLine() in
let rec generatePasswordLetters sofar index lettersLeft =
    (*printfn "generate %s %u %d" sofar index lettersLeft;*)
    if lettersLeft <= 0 then
        sofar
    else
        let collectPasswordLettersFromBatch sofar taskResults =
            let collectPasswordLettersFromTaskResults letter sofar =
                sofar + letter
            in
            List.foldBack collectPasswordLettersFromTaskResults taskResults sofar
        in
        let batchSize = NUM_HASHES_PER_WORKER_INSTANCE in
        let batch = createBatchFindNextInterestingLetter doorId index batchSize in
        let batchResults = Async.RunSynchronously (Async.Parallel batch) in
        let newLetters = Array.fold collectPasswordLettersFromBatch "" batchResults in
        generatePasswordLetters
            (sofar + newLetters.[0 .. (min lettersLeft (String.length newLetters)) - 1])
            (index + (NUM_WORKERS * batchSize))
            (lettersLeft - (String.length newLetters))
;;

measureTime (fun () -> printfn "password: %s" (generatePasswordLetters "" 0UL 8));;
