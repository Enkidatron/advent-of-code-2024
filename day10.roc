app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br",
    parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.9.0/w8YKp2YAgQt5REYk912HfKAHBjcXsrnvtjI0CBzoAT4.tar.br",
    array2d: "https://github.com/mulias/roc-array2d/releases/download/v0.3.1/2Jqajvxn36vRryyQBSluU6Fo6vVI5yNSYmcJcyaKp0Y.tar.br",
}

import pf.Stdout
import parser.Parser
import parser.String exposing [string]
import array2d.Array2D
import array2d.Index2D
import "day10test.txt" as testText : Str
import "day10.txt" as puzzleText : Str

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
    parsed = String.parseStr? mapParser input
    map = Array2D.fromExactLists? parsed
    totalScore = Array2D.walk map 0 { direction: Forwards } \scoreSoFar, num, index ->
        score = if num == 0 then scoreTrailhead map index else 0
        scoreSoFar + score
    Ok totalScore

doPart2 : Str -> Result U64 _
doPart2 = \input ->
    parsed = String.parseStr? mapParser input
    map = Array2D.fromExactLists? parsed
    totalRating = Array2D.walk map 0 { direction: Forwards } \ratingSoFar, num, index ->
        rating = if num == 0 then rateTrailhead map index else 0
        ratingSoFar + rating
    Ok totalRating

mapParser =
    Parser.sepBy lineParser (string "\r\n")
lineParser =
    Parser.many String.digit

scoreTrailhead = \map, trailheadIndex ->
    mapShape = Array2D.shape map
    scoreTrailheadHelp = \currentPositions, elevation ->
        if Set.isEmpty currentPositions then
            currentPositions
        else if elevation >= 9 then
            currentPositions
        else
            nextPositions =
                Set.joinMap currentPositions \position ->
                    List.keepOks [(NextRow, SameCol), (PrevRow, SameCol), (SameRow, NextCol), (SameRow, PrevCol)] \(a, b) -> Index2D.adjacentTo position mapShape a b
                    |> List.keepIf (\newPosition -> Array2D.get map newPosition == Ok (elevation + 1))
                    |> Set.fromList
            scoreTrailheadHelp nextPositions (elevation + 1)
    scoreTrailheadHelp (Set.single trailheadIndex) 0
    |> Set.len
rateTrailhead = \map, trailheadIndex ->
    mapShape = Array2D.shape map
    rateTrailheadHelp = \currentPositions, elevation ->
        if List.isEmpty currentPositions then
            currentPositions
        else if elevation >= 9 then
            currentPositions
        else
            nextPositions =
                List.joinMap currentPositions \position ->
                    List.keepOks [(NextRow, SameCol), (PrevRow, SameCol), (SameRow, NextCol), (SameRow, PrevCol)] \(a, b) -> Index2D.adjacentTo position mapShape a b
                    |> List.keepIf (\newPosition -> Array2D.get map newPosition == Ok (elevation + 1))
            rateTrailheadHelp nextPositions (elevation + 1)
    rateTrailheadHelp (List.single trailheadIndex) 0
    |> List.len
