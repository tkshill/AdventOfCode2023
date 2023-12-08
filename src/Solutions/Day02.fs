namespace Solutions

open Utility
open FParsec
open FParsec.Pipes
open FSharpx.Collections
open System

module Day02 =

    let toSet (red, green, blue) =
        function
        | n, "blue" -> (red, green, blue + n)
        | n, "green" -> (red, green + n, blue)
        | n, "red" -> (red + n, green, blue)
    let pColour = %% +.pint32 -- ' ' -- +.(%["blue"; "red"; "green"]) -%> auto

    let pColours = 
        %% +.(pColour * (qty.[1..] / ", ")) -%> (ResizeArray.toSeq >> Seq.fold toSet (0,0,0))

    let pLine = 
        %% "Game " -- +.pint32 -- ": " -- +.(pColours * (qty.[1..] / "; ")) -- spaces 
        -|> fun n sets -> n, ResizeArray.toList sets

    let isValidSet redlimit greenlimit bluelimit (red, green, blue) =
        if red <= redlimit && blue <= bluelimit && green <= greenlimit then true else false

    let isValidGame sets = 
        sets 
        |> List.filter (isValidSet 12 13 14) 
        |> List.length |> (=) (List.length sets)

    let part1 = runParser pLine >> Seq.filter (snd >> isValidGame) >> Seq.map fst >> Seq.sum

    let required (x, y, z) (red, green, blue) = max x red, max y green, max z blue

    let part2 =
        runParser pLine 
        >> Seq.map (snd >> Seq.fold required (0, 0, 0) >> fun (a, b, c) -> a * b * c ) 
        >> Seq.sum

    let solution = Solution.build (part1, part2)