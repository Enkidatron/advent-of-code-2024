app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br",
    parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.9.0/w8YKp2YAgQt5REYk912HfKAHBjcXsrnvtjI0CBzoAT4.tar.br",
    # array2d: "https://github.com/mulias/roc-array2d/releases/download/v0.3.1/2Jqajvxn36vRryyQBSluU6Fo6vVI5yNSYmcJcyaKp0Y.tar.br",
}

import pf.Stdout
import pf.Utc
import parser.Parser exposing [Parser, skip]
import parser.String exposing [string]
# import array2d.Array2D
import "day20test.txt" as testText : Str
import "day20.txt" as puzzleText : Str

time = \{} -> Utc.now {} |> Task.map Utc.toMillisSinceEpoch
main =
    Stdout.line! "Test 1: $(part1 testText 0)"
    part1Start = time! {}
    Stdout.line! "Part 1: $(part1 puzzleText 100)"
    part1End = time! {}
    Stdout.line! "Part 1 time: $(Num.toStr (part1End - part1Start))ms"
    Stdout.line! "Test 2: $(part2 testText 50)"
    part2Start = time! {}
    Stdout.line! "Part 2: $(part2 puzzleText 100)"
    part2End = time! {}
    Stdout.line! "Part 2 time: $(Num.toStr (part2End - part2Start))ms"

part1 : Str, I64 -> Str
part1 = \input, threshold ->
    when doPart1 input threshold is
        Ok answer -> Num.toStr answer
        Err _ -> "Error"

part2 : Str, I64 -> Str
part2 = \input, threshold ->
    when doPart2 input threshold is
        Ok answer -> Num.toStr answer
        Err _ -> "Error"

doPart1 : Str, I64 -> Result U64 _
doPart1 = \input, threshold ->
    track = String.parseStr? trackParser input
    track
    |> findDistancesWithoutCheating
    |> findCheatsGroupedBySavings
    |> Dict.toList
    |> List.keepIf (\(k, _v) -> k >= threshold)
    |> List.map .1
    |> List.map List.len
    |> List.sum
    |> Ok

doPart2 : Str, I64 -> Result U64 _
doPart2 = \input, threshold ->
    track = String.parseStr? trackParser input
    track
    |> findDistancesWithoutCheating
    |> findBigCheatsGroupedBySavings
    |> Dict.toList
    |> List.keepIf (\(k, _v) -> k >= threshold)
    |> List.map .1
    |> List.sum
    |> Ok

Track : { walls : Set Point, start : Point, end : Point }
Point : { x : I64, y : I64 }

trackParser : Parser _ Track
trackParser =
    Parser.sepBy lineParser (string "\r\n")
    |> Parser.map makeTrack
lineParser =
    Parser.oneOf [
        Parser.const Wall |> skip (string "#"),
        Parser.const Track |> skip (string "."),
        Parser.const Start |> skip (string "S"),
        Parser.const End |> skip (string "E"),
    ]
    |> Parser.many
makeTrack = \lines ->
    emptyTrack = { walls: Set.empty {}, start: { x: 0, y: 0 }, end: { x: 0, y: 0 } }
    List.walkWithIndex lines emptyTrack \outerSoFar, line, y ->
        List.walkWithIndex line outerSoFar \soFar, block, x ->
            point = { x: Num.toI64 x, y: Num.toI64 y }
            when block is
                Wall -> { soFar & walls: Set.insert soFar.walls point }
                Track -> soFar
                Start -> { soFar & start: point }
                End -> { soFar & end: point }

findDistancesWithoutCheating : Track -> Dict Point I64
findDistancesWithoutCheating = \track ->
    getNextPoint = \point, distances ->
        candidates =
            [(-1, 0), (1, 0), (0, -1), (0, 1)]
            |> List.map (\(dx, dy) -> { x: point.x + dx, y: point.y + dy })
            |> List.dropIf (\p -> Set.contains track.walls p)
            |> List.dropIf (\p -> Dict.contains distances p)
        when candidates is
            [newPoint] -> newPoint
            [] -> crash "no candidate points found"
            _ -> crash "too many candidate points found"
    help = \thisPoint, distance, distancesSoFar ->
        nextPoint = getNextPoint thisPoint distancesSoFar
        nextDistance = distance + 1
        updatedDistances = Dict.insert distancesSoFar nextPoint nextDistance
        if nextPoint == track.end then
            updatedDistances
        else
            help nextPoint nextDistance updatedDistances

    help track.start 0 (Dict.single track.start 0)

findCheatsGroupedBySavings : Dict Point I64 -> Dict I64 (List (Point, Point))
findCheatsGroupedBySavings = \distances ->
    Dict.walk distances (Dict.empty {}) \soFar, here, distanceToHere ->
        pointsInCheatRange =
            [(-2, 0), (-1, -1), (0, -2), (1, -1), (2, 0), (1, 1), (0, 2), (-1, 1)]
            |> List.map
                (\(dx, dy) ->
                    x = here.x + dx
                    y = here.y + dy
                    { x, y }
                )
        List.walk pointsInCheatRange soFar \innerSoFar, there ->
            when Dict.get distances there is
                Err _ -> innerSoFar
                Ok distanceToThere ->
                    savings = distanceToHere - (distanceToThere + 2)
                    if savings > 0 then
                        addToCheatCollection innerSoFar savings (there, here)
                    else
                        innerSoFar
addToCheatCollection : Dict I64 (List (Point, Point)), I64, (Point, Point) -> Dict I64 (List (Point, Point))
addToCheatCollection = \dict, key, val ->
    Dict.update dict key \maybeVal ->
        when maybeVal is
            Ok soFar -> List.append soFar val |> Ok
            Err _ -> Ok [val]

findBigCheatsGroupedBySavings : Dict Point I64 -> Dict I64 U64
findBigCheatsGroupedBySavings = \distances ->
    Dict.walk distances (Dict.empty {}) \soFar, here, distanceToHere ->
        pointsInCheatRange = findBigCheats here
        List.walk pointsInCheatRange soFar \innerSoFar, there ->
            when Dict.get distances there is
                Err _ -> innerSoFar
                Ok distanceToThere ->
                    savings = distanceToHere - (distanceToThere + (shortestDistance here there))
                    if savings > 0 then
                        countInCheatCollection innerSoFar savings
                    else
                        innerSoFar
findBigCheats : Point -> List Point
findBigCheats = \here ->
    List.range { start: At -20, end: At 20 }
    |> List.joinMap \x ->
        List.range { start: At -20, end: At 20 }
        |> List.keepOks \y ->
            cheatPoint = { x: here.x + x, y: here.y + y }
            distance = shortestDistance cheatPoint here
            if distance >= 2 && distance <= 20 then
                Ok cheatPoint
            else
                Err OutOfRange
shortestDistance : Point, Point -> I64
shortestDistance = \here, there ->
    Num.absDiff here.x there.x + Num.absDiff here.y there.y

countInCheatCollection : Dict I64 U64, I64 -> Dict I64 U64
countInCheatCollection = \dict, key ->
    Dict.update dict key \maybeVal ->
        when maybeVal is
            Ok soFar -> Ok (soFar + 1)
            Err _ -> Ok 1
