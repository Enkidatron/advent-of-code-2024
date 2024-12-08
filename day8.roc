app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br",
    parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.9.0/w8YKp2YAgQt5REYk912HfKAHBjcXsrnvtjI0CBzoAT4.tar.br",
}

import pf.Stdout
import parser.Parser exposing [Parser]
import parser.String exposing [string]
import "day8test.txt" as testText : Str
import "day8.txt" as puzzleText : Str

main =
    Stdout.line! "Test 1: $(part1 testText)"
    Stdout.line! "Part 1: $(part1 puzzleText)"
    Stdout.line! "Test 2: $(part2 testText)"
    Stdout.line! "Part 2: $(part2 puzzleText)"

part1 : Str -> Str
part1 = \input ->
    when doPart1 input is
        Ok num -> Num.toStr num
        Err _ -> "Error"

part2 : Str -> Str
part2 = \input ->
    when doPart2 input is
        Ok num -> Num.toStr num
        Err _ -> "Error"

doPart1 : Str -> Result U64 _
doPart1 = \input ->
    game = String.parseStr? gameParser input
    game.antennas
    |> Dict.toList
    |> List.map .1
    |> List.joinMap (\antennaPoints -> findAntinodesForPoints antennaPoints game)
    |> Set.fromList
    |> Set.len
    |> Ok

doPart2 : Str -> Result U64 _
doPart2 = \input ->
    game = String.parseStr? gameParser input
    game.antennas
    |> Dict.toList
    |> List.map .1
    |> List.joinMap (\antennaPoints -> findAntinodesForPoints2 antennaPoints game)
    |> Set.fromList
    |> Set.len
    |> Ok

Point : { line : U64, col : U64 }
Game : { antennas : Dict Str (List Point), width : U64, height : U64 }

gameParser : Parser _ Game
gameParser =
    Parser.sepBy lineParser (string "\r\n")
    |> Parser.map gameFromLines
lineParser =
    Parser.oneOf [
        Parser.const Empty |> Parser.skip (string "."),
        String.codeunitSatisfies isNotNewLine |> Parser.map String.strFromAscii |> Parser.map Antenna,
    ]
    |> Parser.many
isNotNewLine = \char -> char != '\r'
gameFromLines : List (List [Empty, Antenna Str]) -> Game
gameFromLines = \lines ->
    antennas = List.walkWithIndex lines (Dict.empty {}) \soFar, line, lineNum ->
        List.walkWithIndex line soFar \innerSoFar, item, colNum ->
            when item is
                Empty -> innerSoFar
                Antenna label ->
                    Dict.update innerSoFar label \maybeVal ->
                        when maybeVal is
                            Ok points -> Ok (List.append points (makePoint lineNum colNum))
                            Err _ -> Ok [makePoint lineNum colNum]
    width = lines |> List.map List.len |> List.max |> Result.withDefault 0
    height = List.len lines
    { antennas, width, height }
makePoint = \line, col ->
    { line, col }

Offset : { lineOffset : I64, colOffset : I64 }
findAntinodesForPoints : List Point, Game -> List Point
findAntinodesForPoints = \antennaPoints, game ->
    when antennaPoints is
        [] -> []
        [_] -> []
        [point, .. as otherPoints] ->
            List.joinMap otherPoints \otherPoint ->
                offset = findOffset point otherPoint
                List.keepOks
                    [
                        applyOffset otherPoint offset,
                        applyOffset point (invertOffset offset),
                    ]
                    \mp -> mp
                |> List.dropIf \p -> p.line >= game.height || p.col >= game.width
                |> List.concat (findAntinodesForPoints otherPoints game)

findOffset : Point, Point -> Offset
findOffset = \a, b ->
    lineOffset = (Num.toI64 b.line) - (Num.toI64 a.line)
    colOffset = (Num.toI64 b.col) - (Num.toI64 a.col)
    { lineOffset, colOffset }

applyOffset : Point, Offset -> Result Point _
applyOffset = \point, offset ->
    line = (Num.toI64 point.line) + offset.lineOffset |> Num.toU64Checked?
    col = (Num.toI64 point.col) + offset.colOffset |> Num.toU64Checked?
    Ok { line, col }

invertOffset : Offset -> Offset
invertOffset = \offset ->
    { lineOffset: offset.lineOffset * -1, colOffset: offset.colOffset * -1 }

findAntinodesForPoints2 : List Point, Game -> List Point
findAntinodesForPoints2 = \antennaPoints, game ->
    when antennaPoints is
        [] -> []
        [point] -> [point]
        [point, .. as otherPoints] ->
            List.joinMap otherPoints \otherPoint ->
                offset = findOffset point otherPoint
                hereToThere = keepWalkingByOffset [] otherPoint offset game
                thereToHere = keepWalkingByOffset [] point (invertOffset offset) game
                List.single point
                |> List.concat hereToThere
                |> List.concat thereToHere
                |> List.concat (findAntinodesForPoints2 otherPoints game)

keepWalkingByOffset : List Point, Point, Offset, Game -> List Point
keepWalkingByOffset = \soFar, point, offset, game ->
    when applyOffset point offset is
        Err OutOfBounds -> soFar
        Ok newPoint ->
            if newPoint.line >= game.height || newPoint.col >= game.width then
                soFar
            else
                keepWalkingByOffset (List.append soFar newPoint) newPoint offset game
