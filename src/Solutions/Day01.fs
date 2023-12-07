namespace Solutions

open Utility
open FParsec
open FParsec.Pipes
open FSharpx.Collections
open System

module Day1 =
    // Part 1. Using Fparsec and parser combinators
    let processLine strs = strs |> (fun s -> Seq.head s + Seq.last s) |> int

    let pDigit =
        %%(asciiLetter * qty.[0..]) -- +.digit -- (asciiLetter * qty.[0..]) -|> string

    let pLine =
        %% +.(pDigit * qty.[1..]) -- spaces -|> (ResizeArray.toSeq >> Seq.map string >> processLine)

    let part1 = runParser pLine >> Seq.sum

    // Part 2. Good ol' recursion, and Active Patterns!

    let (|FromText|_|) tst (txt: string list) =
        match String.concat "" (Seq.truncate (String.length tst) txt) = tst with
        | true -> Some tst
        | false -> None

    let (|Digit|_|) (head :: tail) =
        match head :: tail with
        | digit when Char.IsDigit(char head) -> Some head
        | FromText "one" _ -> Some "1"
        | FromText "two" _ -> Some "2"
        | FromText "three" _ -> Some "3"
        | FromText "four" _ -> Some "4"
        | FromText "five" _ -> Some "5"
        | FromText "six" _ -> Some "6"
        | FromText "seven" _ -> Some "7"
        | FromText "eight" _ -> Some "8"
        | FromText "nine" _ -> Some "9"
        | _ -> None

    let rec parseLine (digits, txt) =
        match txt with
        | [] -> digits
        | Digit digit -> parseLine (digits @ [digit], List.tail txt)
        | _ -> parseLine (digits, List.tail txt)

    let part2 =
        Seq.map (Seq.toList >> List.map string >> toTuple [] >> parseLine >> List.toSeq >> processLine) >> Seq.sum

    let solution = Solution.build (part1, part2)
