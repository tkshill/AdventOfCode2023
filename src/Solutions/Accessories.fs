[<AutoOpen>]
module rec Utility

open System.IO
open FParsec
open FParsec.Pipes
open FSharpx.Collections

[<AutoOpen>]
module TupleHelpers =
    let unpack f (a, b) = f a b

    let unpack2 f (a, b, c) = f a b c

    let pack f a b = f (a, b)

    let tupleMap f f2 (a, b) = (f a, f2 b)

    let toTuple a b = a, b

    let trd (_, _, z) = z

    let tupleFold f f2 (a, b) = f a, f2 (f a ) b

    let seqToTuple sequence = (Seq.head sequence, Seq.last sequence)

[<AutoOpen>]
module Potpourri =
    let flip f a b = f b a
    let flip2 f a b c = f c a b

    let boolToInt b = if b then 1 else 0

    let inc v = v + 1

    let dec v = v - 1

    let cond condition ifTrue ifFalse = if condition then ifTrue else ifFalse

    let notEqualTo value = ((<>) value)

    let isnt f = f >> not

    let splitByChars (chars) (s: string) = s.Split(chars)

    let splitByString (str: string) (s: string) = s.Split(str)

    let toString (n: char array) = new System.String(n)

[<AutoOpen>]
module Solution =
    type Part = (string[]) -> string

    let dummyPart: Part = fun _ -> failwith "Not implemented yet"

type Solution(input: string[], firstPart: Part, secondPart: Part) =
    let part1 = firstPart
    let part2 = secondPart
    let data = input
    member this.Part1(?input) = defaultArg input data |> part1
    member this.Part2(?input) = defaultArg input data |> part2

    static member build(?firstPart, ?secondPart) =
        let part1 = firstPart |> Option.map (flip (>>) string) |> flip defaultArg dummyPart
        let part2 = secondPart |> Option.map (flip (>>) string) |> flip defaultArg dummyPart
        fun input -> Solution(input, part1, part2)

type MaybeBuilder() =
    member this.Bind(x, f) =
        match x with
        | Some x -> f x
        | _ -> None

    member this.Zero() = None

let maybe = new MaybeBuilder()

[<AutoOpen>]
module SeqHelpers =
    let splitBy condition sequence =
        (Seq.takeWhile condition sequence, Seq.skipWhile condition sequence)

    let dropLast sequence =
        Seq.removeAt (Seq.length sequence - 1) sequence

    let rec trimEnds sequence =
        match sequence with
        | [||]
        | [| "" |] -> Array.empty
        | x when Array.head x = "" -> trimEnds (Array.tail x)
        | x when Array.last x = "" -> trimEnds (x[.. (Array.length x - 2)])
        | _ -> sequence

    let seqFrom2D (a: 'a[,]) : seq<'a> =
        seq {
            for x in a do
                yield x :?> 'a
        }

    let indexWithOffset (n: int) (s: 'a seq) =
        s |> Seq.indexed |> Seq.map (tupleMap ((+) n) id)

    let infiniteNext s value =
        let index = Seq.findIndex (fun v -> v = value) s

        if index = Seq.length s - 1 then
            Seq.head s
        else
            Seq.item (index + 1) s

[<AutoOpen>]
module Effects =
    let log transformer value =
        value

    let getInput dayNumber =
        let fileName = $"./data/day{dayNumber}.txt"

        if File.Exists(fileName) then
            Some(File.ReadAllLines fileName |> trimEnds)
        else
            None

    let withEffect (f: 'a -> unit) (v: 'a) =
        f v
        v

[<AutoOpen>]
module Parser =
    let integerParser: Parser<int, unit> =
        %% +.(qty.[1..] * digit)
        -|> (ResizeArray.toArray >> System.String >> System.Int32.Parse)

    let parserResultToOption =
        function
        | Success(value, _, _) -> Some value
        | _ -> None

    let pT (sep: string) : Parser<int * int, unit> = %% +.pint32 -- sep -- +.pint32 -%> auto

    let runParser parser =
        Seq.choose (run parser >> parserResultToOption)