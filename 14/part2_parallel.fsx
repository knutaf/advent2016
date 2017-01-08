open System;;
open System.Security.Cryptography;;
open System.Threading;;
open QueryPerformance;;

exception Ex of string;;

type RingBuffer<'a>(size, initializer) =
    let storage = Array.create size initializer
    let mutable readIndex = 0
    let mutable writeIndex = 0

    member this.append value =
        let work () =
            let nextWriteIndex = (writeIndex + 1) % (Array.length storage) in
            if nextWriteIndex = readIndex then
                false
            else
                let _ = storage.[writeIndex] <- value in
                let _ = writeIndex <- nextWriteIndex in
                true
        in
        lock storage work

    member this.peek () =
        if readIndex = writeIndex then
            None
        else
            Some storage.[readIndex]

    member this.consumeFirst () =
        let work () =
            let itemOpt = this.peek () in
            let _ =
                if itemOpt.IsSome then
                    readIndex <- (readIndex + 1) % (Array.length storage)
                else
                    ()
            in
            itemOpt
        in
        lock storage work
;;

let toByteArray str =
    Seq.toArray (Seq.map (fun (elem : char) -> Convert.ToByte(elem)) str)
;;

let BYTE_TO_STRING = [| for i in 0 .. 0xff -> sprintf "%02x" i |];;
let BYTE_TO_BYTE_STRING = Array.map toByteArray BYTE_TO_STRING;;

let toHexString byteArray =
    Array.fold (fun state (elem : byte) -> state + BYTE_TO_STRING.[int elem]) "" byteArray
;;

type Key = { index : uint64; nibble : byte; bytes : byte[] };;

let byteArrayToStringByteArray input output =
    assert ((Array.length output) = (Array.length input) * 2);
    Array.iteri (fun i b ->
        let outputIndex = i * 2 in
        let nibbles = BYTE_TO_BYTE_STRING.[int b] in
        let _ = output.[outputIndex] <- nibbles.[0] in
        output.[outputIndex + 1] <- nibbles.[1]
    ) input;
    (*printfn "%s -> %s" (toHexString input) (toHexString output)*)
    output
;;

let findFirstNibbleSequence numConsecutive nibble bytes =
    let addConsecutiveNibble (startingPoint, runSoFar, currentNibbleIndex) b =
        (*let _ = printfn "%02x (%01x %01x) - start %d, run %d, idx %d" b ((b >>> 4) &&& 0xfuy) (b &&& 0xfuy) startingPoint runSoFar currentNibbleIndex in*)
        let nextNibbleIndex = currentNibbleIndex + 2 in
        let highNibbleMatches = ((b >>> 4) &&& 0xfuy) = nibble in
        let lowNibbleMatches = (b &&& 0xfuy) = nibble in
        if runSoFar < numConsecutive then
            let (startingPoint1, runSoFar1) =
                if highNibbleMatches then
                    if runSoFar = 0 then
                        (currentNibbleIndex, runSoFar + 1)
                    else
                        (startingPoint, runSoFar + 1)
                else
                    (0, 0)
            in
            if runSoFar1 >= numConsecutive then
                (startingPoint1, runSoFar1, nextNibbleIndex)
            elif lowNibbleMatches then
                if runSoFar1 = 0 then
                    (currentNibbleIndex + 1, runSoFar1 + 1, nextNibbleIndex)
                else
                    (startingPoint1, runSoFar1 + 1, nextNibbleIndex)
            else
                (0, 0, nextNibbleIndex)
        else
            let _ = assert(runSoFar = numConsecutive) in
            (startingPoint, runSoFar, nextNibbleIndex)
    in
    let (runStart, runLength, _) = Array.fold addConsecutiveNibble (0, 0, 0) bytes in
    (runStart, runLength)
;;

let CANDIDATE_SEQUENCE_LENGTH = 3;;
let CONFIRM_SEQUENCE_LENGTH = 5;;
let CONFIRM_INDEX_RANGE = 1000UL;;
let HASH_BYTE_LENGTH = 16;;

let createCandidateKey index bytes =
    let findEarliestNibbleSequence (earliestSequenceStart, earliestSequenceNibble) nibble =
        let (runStart, runLength) = findFirstNibbleSequence CANDIDATE_SEQUENCE_LENGTH nibble bytes in
        if runLength = CANDIDATE_SEQUENCE_LENGTH then
            if earliestSequenceStart = -1 then
                (*let _ = printfn "found first candidate sequence at %d, nibble %1x" runStart nibble in*)
                (runStart, nibble)
            elif runStart < earliestSequenceStart then
                (*let _ = printfn "found new first candidate sequence at %d, nibble %1x" runStart nibble in*)
                (runStart, nibble)
            else
                let _ = assert(runStart <> earliestSequenceStart) in
                (earliestSequenceStart, earliestSequenceNibble)
        else
            (earliestSequenceStart, earliestSequenceNibble)
    in
    let (earliestSequenceStart, earliestSequenceNibble) = Seq.fold findEarliestNibbleSequence (-1, 0uy) (seq { 0uy .. 0xfuy }) in
    if earliestSequenceStart <> -1 then
        (*let _ = printfn "using candidate with sequence at %d, nibble %1x" earliestSequenceStart earliestSequenceNibble in*)
        Some { index = index; nibble = earliestSequenceNibble; bytes = bytes }
    else
        (*let _ = printfn "no sequence found" in*)
        None
;;

let g_nextIndex = ref 0L;;

let mutable g_numHashesToStorePerWorker = 0;;
let mutable g_numWorkers = 0;;
let mutable g_useSpinWaits = false;;
let mutable g_hashStorage = Array.empty<RingBuffer<uint64 * byte[]>>;;
let g_hashReadyEvent = new AutoResetEvent(false);;

let mutable g_perfCountsContendingNextIndex = Array.empty<uint64>;;
let mutable g_perfCountsContendingStoreHash = Array.empty<uint64>;;
let mutable g_perfCountsContendingConsumeHash = 0UL;;

let initializeHashStorage () =
    assert (g_numWorkers > 0);
    assert (g_numHashesToStorePerWorker > 0);
    g_hashStorage <- [| for i in 1 .. g_numWorkers -> new RingBuffer<uint64 * byte[]>(g_numHashesToStorePerWorker, (0UL, Array.empty)) |];
    g_perfCountsContendingNextIndex <- Array.create g_numWorkers 0UL;
    g_perfCountsContendingStoreHash <- Array.create g_numWorkers 0UL;
;;

let createHasher workerNum stretchSize salt =
    async {
        let md5 = MD5.Create("MD5") in
        let intermediateHashBuffer = Array.create (HASH_BYTE_LENGTH * 2) 0uy in
        let generateStretchedHash index =
            (*let _ = printfn "%d generating index %u" workerNum index in*)
            let rec helper stretchSize (bytes:byte[]) =
                let md5Bytes = md5.ComputeHash(bytes) in
                if stretchSize = 0 then
                    md5Bytes
                else
                    helper (stretchSize - 1) (byteArrayToStringByteArray md5Bytes intermediateHashBuffer)
            in
            helper stretchSize (toByteArray (sprintf "%s%u" salt index))
        in
        let getNextIndex () =
            let startCount = QueryPerformance.QueryPerformanceCounter () in
            let nextIndex = (Microsoft.FSharp.Core.Operators.uint64 (Interlocked.Increment(g_nextIndex))) - 1UL in
            let endCount = QueryPerformance.QueryPerformanceCounter () in
            let _ = g_perfCountsContendingNextIndex.[workerNum] <- g_perfCountsContendingNextIndex.[workerNum] + (endCount - startCount) in
            nextIndex
        in
        let rec generateAndStoreNextHash index =
            let hsh = generateStretchedHash index in
            let rec storeHash attemptNum =
                (*
                let _ =
                    if attemptNum > 1 then
                        printfn "%d: storeHash %u attempt %d" workerNum index attemptNum
                    else
                        ()
                in
                *)
                let startCount = QueryPerformance.QueryPerformanceCounter () in
                let succeededAppend = g_hashStorage.[workerNum].append (index, hsh) in
                let endCount = QueryPerformance.QueryPerformanceCounter () in
                let _ = g_perfCountsContendingStoreHash.[workerNum] <- g_perfCountsContendingStoreHash.[workerNum] + (endCount - startCount) in
                if succeededAppend then
                    if g_useSpinWaits then
                        ()
                    else
                        let _ = g_hashReadyEvent.Set() in
                        ()
                else
                    storeHash (attemptNum + 1)
            in
            let _ = storeHash 1 in
            generateAndStoreNextHash (getNextIndex ())
        in
        generateAndStoreNextHash (getNextIndex ())
    }
;;

let consumeNextHash index =
    let rec helper attemptNum =
        (*
        let _ =
            if (attemptNum % 1000) = 0 then
                ()
                printfn "consumeNextHash %u attempt %d" index attemptNum
            else
                ()
        in
        *)
        let tryConsumeHashFromRingBuffer (hashOpt:byte[] option) (ringBuffer:RingBuffer<uint64 * byte[]>) =
            if hashOpt.IsNone then
                match ringBuffer.peek () with
                | Some (indexInRingBuffer, hashInRingBuffer) ->
                    if indexInRingBuffer = index then
                        let startCount = QueryPerformance.QueryPerformanceCounter () in
                        let consumed = ringBuffer.consumeFirst () in
                        let endCount = QueryPerformance.QueryPerformanceCounter () in
                        let _ = g_perfCountsContendingConsumeHash <- g_perfCountsContendingConsumeHash + (endCount - startCount) in
                        let _ = assert(consumed.IsSome) in
                        Some hashInRingBuffer
                    else
                        hashOpt
                | None -> hashOpt
            else
                hashOpt
        in
        match Array.fold tryConsumeHashFromRingBuffer None g_hashStorage with
        | None ->
            let _ =
                if g_useSpinWaits then
                    ()
                else
                    let _ = g_hashReadyEvent.WaitOne() in
                    ()
            in
            helper (attemptNum + 1)
        | Some hsh -> hsh
    in
    helper 1
;;

let generateKeys salt numKeysToGenerate stretchSize =
    let rec helper keysSoFar keysSoFarLength candidateKeys perfCountsWaitingForHashes index =
        let _ =
            if (index % 1000UL) = 0UL then
                printfn "index %u, keys so far %d, num candidates %d" index keysSoFarLength (List.length candidateKeys)
            else
                ()
        in
        let startCount = QueryPerformance.QueryPerformanceCounter () in
        let md5Bytes = consumeNextHash index in
        let endCount = QueryPerformance.QueryPerformanceCounter () in
        let newPerfCountsWaitingForHashes = perfCountsWaitingForHashes + (endCount - startCount) in
        let md5BytesString = toHexString md5Bytes in
        (*let _ = printfn "index %u, hash %s, num candidates %d" index md5BytesString (List.length candidateKeys) in*)
        let tryConfirmCandidateKey (newKeysSoFar, newKeysSoFarLength, newCandidateKeys) candidateKey =
            if index <= candidateKey.index + CONFIRM_INDEX_RANGE then
                let (_, seqLength) = findFirstNibbleSequence CONFIRM_SEQUENCE_LENGTH candidateKey.nibble md5Bytes in
                if seqLength = CONFIRM_SEQUENCE_LENGTH then
                    (*let _ = printfn "confirming candidate index %u - %s with %s at index %u" candidateKey.index (toHexString candidateKey.bytes) md5BytesString index in*)
                    (candidateKey :: newKeysSoFar, newKeysSoFarLength + 1, newCandidateKeys)
                else
                    (newKeysSoFar, newKeysSoFarLength, candidateKey :: newCandidateKeys)
            else
                (*let _ = printfn "at index %u, aging out candidate index %u" index candidateKey.index in*)
                (newKeysSoFar, newKeysSoFarLength, newCandidateKeys)
        in
        let (confirmedKeysSoFar, confirmedKeysSoFarLength, remainingCandidateKeys) = List.fold tryConfirmCandidateKey (keysSoFar, keysSoFarLength, []) candidateKeys in
        if confirmedKeysSoFarLength < numKeysToGenerate then
            let newCandidateKeys =
                match createCandidateKey index md5Bytes with
                | Some candidateKey -> candidateKey :: remainingCandidateKeys
                | None -> remainingCandidateKeys
            in
            helper confirmedKeysSoFar confirmedKeysSoFarLength newCandidateKeys newPerfCountsWaitingForHashes (index + 1UL)
        elif not (List.isEmpty remainingCandidateKeys) then
            helper confirmedKeysSoFar confirmedKeysSoFarLength remainingCandidateKeys newPerfCountsWaitingForHashes (index + 1UL)
        else
            let _ = printfn "ms spent waiting for hashes: %u" (QueryPerformance.millisecondsFromPerfCounts newPerfCountsWaitingForHashes) in
            let _ = printfn "ms spent waiting on getNextIndex: %u" (QueryPerformance.millisecondsFromPerfCounts (Array.sum g_perfCountsContendingNextIndex)) in
            let _ = printfn "ms spent waiting on append: %u" (QueryPerformance.millisecondsFromPerfCounts (Array.sum g_perfCountsContendingStoreHash)) in
            let _ = printfn "ms spent waiting on consumeFirst: %u" (QueryPerformance.millisecondsFromPerfCounts g_perfCountsContendingConsumeHash) in
            confirmedKeysSoFar
    in
    let _ = initializeHashStorage () in
    let hashWorkers = [ for i in 0 .. (g_numWorkers - 1) -> createHasher i stretchSize salt ] in
    let _ = List.iter Async.Start hashWorkers in
    helper [] 0 [] 0UL 0UL
;;

[<EntryPoint>]
let main argv =
    match argv with
    | [|salt; numKeysStr; stretchSizeStr; numWorkersStr; numHashesToStorePerWorkerStr; useSpinWaits |] ->
        let numKeys = Convert.ToInt32(numKeysStr) in
        let stretchSize = Convert.ToInt32(stretchSizeStr) in
        let _ = g_numWorkers <- Convert.ToInt32(numWorkersStr) in
        let _ = g_numHashesToStorePerWorker <- Convert.ToInt32(numHashesToStorePerWorkerStr) in
        let _ = g_useSpinWaits <- (useSpinWaits = "true") in
        let keys = generateKeys salt numKeys stretchSize in
        let sortedKeys = List.sortBy (fun elem -> elem.index) keys in
        let _ = List.mapi (fun i elem -> printfn "key %2d - index: %7u, %s" (i + 1) elem.index (toHexString elem.bytes)) sortedKeys in
        ()
    | _ -> printfn "need salt, num keys, stretch size str, num workers, num hashes to store per worker"
    0
;;
