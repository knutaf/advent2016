open System.Runtime.InteropServices;;

module QueryPerformance =
    module Wrapper =
        [<DllImport("KERNEL32.dll")>]
        extern bool QueryPerformanceCounter(uint64& wpPerformanceCount)

        [<DllImport("KERNEL32.dll")>]
        extern bool QueryPerformanceFrequency(uint64& lpFrequency)

    let mutable frequency =
        let mutable result = 0UL in
        if Wrapper.QueryPerformanceFrequency(&result) then
            result
        else
            0UL

    let QueryPerformanceCounter () =
        let mutable result = 0UL in
        if Wrapper.QueryPerformanceCounter(&result) then
            result
        else
            0UL

    let getMillisecondsElapsed startCount endCount =
        1000UL * (endCount - startCount) / frequency
;;
