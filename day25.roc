app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br",
    parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.9.0/w8YKp2YAgQt5REYk912HfKAHBjcXsrnvtjI0CBzoAT4.tar.br",
    array2d: "https://github.com/mulias/roc-array2d/releases/download/v0.3.1/2Jqajvxn36vRryyQBSluU6Fo6vVI5yNSYmcJcyaKp0Y.tar.br",
}

import pf.Stdout
import pf.Utc
import parser.Parser exposing [Parser, const, skip]
import parser.String exposing [string]
import array2d.Array2D exposing [Array2D]
import "day25test.txt" as testText : Str
import "day25.txt" as puzzleText : Str

time = \{} -> Utc.now {} |> Task.map Utc.toMillisSinceEpoch
main =
    Stdout.line! "Test 1: $(part1 testText)"
    part1Start = time! {}
    Stdout.line! "Part 1: $(part1 puzzleText)"
    part1End = time! {}
    Stdout.line! "Part 1 time: $(Num.toStr (part1End - part1Start))ms"
# Stdout.line! "Test 2: $(part2 testText)"
# part2Start = time! {}
# Stdout.line! "Part 2: $(part2 puzzleText)"
# part2End = time! {}
# Stdout.line! "Part 2 time: $(Num.toStr (part2End - part2Start))ms"

part1 : Str -> Str
part1 = \input ->
    when doPart1 input |> dbg is
        Ok answer -> Num.toStr answer
        Err _ -> "Error"

part2 : Str -> Str
part2 = \input ->
    when doPart2 input is
        Ok answer -> Num.toStr answer
        Err _ -> "Error"

doPart1 : Str -> Result U64 _
doPart1 = \input ->
    schematics = String.parseStr? schematicsParser input
    countMatchingSchematics schematics
    |> Ok

doPart2 : Str -> Result U64 _
doPart2 = \input ->
    Err NotDoneYet

Schematic : Array2D [Empty, Occupied]

schematicsParser : Parser _ (List Schematic)
schematicsParser =
    schematicParser |> Parser.sepBy (string "\r\n\r\n")

schematicParser : Parser _ Schematic
schematicParser =
    lineParser
    |> Parser.sepBy (string "\r\n")
    |> Parser.map Array2D.fromExactLists
    |> Parser.map (\r -> Result.mapErr r (\_ -> "bad lists"))
    |> Parser.flatten
lineParser =
    Parser.oneOf [
        const Empty |> skip (string "."),
        const Occupied |> skip (string "#"),
    ]
    |> Parser.oneOrMore

countMatchingSchematics : List Schematic -> U64
countMatchingSchematics = \schematics ->
    schematicsMatch = \left, right ->
        Array2D.walkUntil left Bool.true ({ direction: Forwards }) \_, leftBlock, index ->
            when Array2D.get right index is
                Ok rightBlock ->
                    if leftBlock == Occupied && rightBlock == Occupied then
                        Break Bool.false
                    else
                        Continue Bool.true

                Err OutOfBounds ->
                    Break Bool.false

    countMatches = \first, rest ->
        List.countIf rest (\other -> schematicsMatch first other)
    help = \list, countSoFar ->
        when list is
            [] | [_last] -> countSoFar
            [first, .. as rest] ->
                help rest (countSoFar + countMatches first rest)
    help schematics 0
