open Argu
open System
open Solutions

open Utility

let getSolution input : string -> Solution option =
    function
    | "1" -> Some(Day1.solution input)
    // | "2" -> Some(Day02.solution input)
    // | "3" -> Some(Day03.solution input)
    // | "4" -> Some(Day04.solution input)
    // | "5" -> Some(Day05.solution input)
    // | "6" -> Some(Day06.solution input)
    // | "7" -> Some(Day07.solution input)
    // | "8" -> Some(Day08.solution input)
    // | "9" -> Some(Day09.solution input)
    // | "10" -> Some(Day10.solution input)
    // | "11" -> Some(Day11.solution input)
    // | "13" -> Some(Day13.solution input)
    // | "14" -> Some(Day14.solution input)
    // | "15" -> Some(Day15.solution input)
    // | "16" -> Some(Day16.solution input)
    | _ -> None

[<EntryPoint>]
let main args =
    maybe {
        let! day = Seq.tryHead args
        let! part = Seq.tryItem 1 args
        let! input = getInput day
        let! solution = getSolution input day

        match part with
        | "1" -> printfn "%A" (solution.Part1())
        | "2" -> printfn "%A" (solution.Part2())
        | _ ->
            printfn "%A" (solution.Part1())
            printfn "%A" (solution.Part2())
    }
    |> ignore

    0