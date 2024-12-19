app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br",
    parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.9.0/w8YKp2YAgQt5REYk912HfKAHBjcXsrnvtjI0CBzoAT4.tar.br",
    # array2d: "https://github.com/mulias/roc-array2d/releases/download/v0.3.1/2Jqajvxn36vRryyQBSluU6Fo6vVI5yNSYmcJcyaKp0Y.tar.br",
}

import pf.Stdout
import pf.Utc
import parser.Parser exposing [Parser, keep, skip]
import parser.String exposing [string, digits]
# import array2d.Array2D
import "day18test.txt" as testText : Str
import "day18.txt" as puzzleText : Str

time = \{} -> Utc.now {} |> Task.map Utc.toMillisSinceEpoch
main =
    Stdout.line! "Test 1: $(part1 testText TestArena)"
    part1Start = time! {}
    Stdout.line! "Part 1: $(part1 puzzleText RealArena)"
    part1End = time! {}
    Stdout.line! "Part 1 time: $(Num.toStr (part1End - part1Start))ms"
    Stdout.line! "Test 2: $(part2 testText TestArena)"
    part2Start = time! {}
    Stdout.line! "Part 2: $(part2 puzzleText RealArena)"
    part2End = time! {}
    Stdout.line! "Part 2 time: $(Num.toStr (part2End - part2Start))ms"

part1 : Str, _ -> Str
part1 = \input, arena ->
    when doPart1 input arena is
        Ok answer -> Num.toStr answer
        Err _ -> "Error"

part2 : Str, Arena -> Str
part2 = \input, arena ->
    when doPart2 input arena is
        Ok point -> "$(Num.toStr point.x),$(Num.toStr point.y)"
        Err _ -> "Error"

doPart1 : Str, Arena -> Result I64 _
doPart1 = \input, arena ->
    game = String.parseStr? (gameParser arena) input
    findQuickestExit game

doPart2 : Str, Arena -> Result Point _
doPart2 = \input, arena ->
    game = String.parseStr? (gameParser arena) input
    findBlockingPoint game
Arena : [TestArena, RealArena]
Game : { corrupted : Set Point, size : I64, pointsToFall : List Point }
Point : { x : I64, y : I64 }
gameParser : Arena -> Parser _ Game
gameParser = \arena ->
    Parser.map pointsParser \points -> {
        corrupted: Set.fromList (keepPointsForArenaSize points arena),
        pointsToFall: dropPointsForArenaSize points arena,
        size: sizeForArena arena,
    }
pointsParser =
    Parser.sepBy pointParser (string "\r\n")
pointParser =
    Parser.const (\x -> \y -> { x, y })
    |> keep (digits |> Parser.map Num.toI64)
    |> skip (string ",")
    |> keep (digits |> Parser.map Num.toI64)
keepPointsForArenaSize = \points, arena ->
    numToKeep =
        when arena is
            TestArena -> 12
            RealArena -> 1024
    List.takeFirst points numToKeep
dropPointsForArenaSize = \points, arena ->
    numToDrop =
        when arena is
            TestArena -> 12
            RealArena -> 1024
    List.dropFirst points numToDrop
sizeForArena = \arena ->
    when arena is
        TestArena -> 6
        RealArena -> 70

findQuickestExit : Game -> Result I64 [NoExit]
findQuickestExit = \game ->
    exitPoint = { x: game.size, y: game.size }
    help = \frontiers, cheapestByPoint ->
        when frontiers is
            [] ->
                Err NoExit

            [cheapest, .. as rest] ->
                if cheapest.location == exitPoint then
                    Ok cheapest.steps
                else
                    ourNextMoves =
                        nextMoves cheapest
                        |> List.keepIf (\move -> locationIsInBounds move.location game.size)
                        |> List.dropIf (\move -> Set.contains game.corrupted move.location)
                        |> List.keepIf
                            (\move ->
                                maybeBest = Dict.get cheapestByPoint move.location
                                when maybeBest is
                                    Err _ -> Bool.true
                                    Ok best -> move.steps < best
                            )
                    newCheapestByPoint = List.walk ourNextMoves cheapestByPoint \soFar, move -> Dict.insert soFar move.location move.steps
                    ourNextMoves
                    |> List.concat rest
                    |> List.sortWith (\a, b -> Num.compare (score a exitPoint) (score b exitPoint))
                    |> help newCheapestByPoint
    help [{ location: { x: 0, y: 0 }, steps: 0 }] (Dict.single { x: 0, y: 0 } 0)

locationIsInBounds = \location, gameSize ->
    location.x >= 0 && location.x <= gameSize && location.y >= 0 && location.y <= gameSize

nextMoves = \move ->
    [(-1, 0), (1, 0), (0, -1), (0, 1)]
    |> List.map \(dx, dy) -> { location: { x: move.location.x + dx, y: move.location.y + dy }, steps: move.steps + 1 }

score = \move, exitPoint ->
    move.steps - (exitPoint.x - move.location.x) - (exitPoint.y - move.location.y)

# Part 2

findQuickestExit2 : Game -> Result _ [NoExit]
findQuickestExit2 = \game ->
    exitPoint = { x: game.size, y: game.size }
    help = \frontiers, cheapestByPoint ->
        when frontiers is
            [] ->
                Err NoExit

            [cheapest, .. as rest] ->
                if cheapest.location == exitPoint then
                    Ok cheapest
                else
                    ourNextMoves =
                        nextMoves2 cheapest
                        |> List.keepIf (\move -> locationIsInBounds move.location game.size)
                        |> List.dropIf (\move -> Set.contains game.corrupted move.location)
                        |> List.keepIf
                            (\move ->
                                maybeBest = Dict.get cheapestByPoint move.location
                                when maybeBest is
                                    Err _ -> Bool.true
                                    Ok best -> move.steps < best
                            )
                    newCheapestByPoint = List.walk ourNextMoves cheapestByPoint \soFar, move -> Dict.insert soFar move.location move.steps
                    ourNextMoves
                    |> List.concat rest
                    |> List.sortWith (\a, b -> Num.compare (score a exitPoint) (score b exitPoint))
                    |> help newCheapestByPoint
    help [{ location: { x: 0, y: 0 }, steps: 0, visited: Set.empty {} }] (Dict.single { x: 0, y: 0 } 0)

nextMoves2 = \move ->
    [(-1, 0), (1, 0), (0, -1), (0, 1)]
    |> List.map \(dx, dy) -> { location: { x: move.location.x + dx, y: move.location.y + dy }, steps: move.steps + 1, visited: Set.insert move.visited move.location }

findBlockingPoint = \game ->
    bestPath = findQuickestExit2? game
    help = \currentGame, currentBestPath ->
        when currentGame.pointsToFall is
            [] -> Err CanAlwaysExit
            [nextCorruption, .. as rest] ->
                nextGame = { currentGame & corrupted: Set.insert currentGame.corrupted nextCorruption, pointsToFall: rest }
                if Set.contains currentBestPath.visited nextCorruption then
                    when findQuickestExit2 nextGame is
                        Err NoExit ->
                            Ok nextCorruption

                        Ok newBestPath ->
                            help nextGame newBestPath
                else
                    help nextGame currentBestPath
    help game bestPath

# debugMap = \game, path ->
#     List.range {start: At 0, end: At game.size} |> List.map \lineNum ->
#         List.range {start: At 0, end: At game.size} |> List.map \colNum ->
#             point = {x: colNum, y: lineNum}
#             if Set.contains game.corrupted point then
#                 "#"
#             else if Set.contains path.visited point then
#                 "O"
#             else if point == {x: game.size, y: game.size} then
#                 "X"
#             else "."
#         |> Str.joinWith ""
#     |> Str.joinWith "\r\n"
#     |> \map -> "\r\n$(map)"
#     |> dbg
