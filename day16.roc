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
import "day16test.txt" as testText : Str
import "day16.txt" as puzzleText : Str

time = \{} -> Utc.now {} |> Task.map Utc.toMillisSinceEpoch
main =
    Stdout.line! "Test 1: $(part1 testText)"
    part1Start = time! {}
    Stdout.line! "Part 1: $(part1 puzzleText)"
    part1End = time! {}
    Stdout.line! "Part 1 time: $(Num.toStr (part1End - part1Start))ms"
    Stdout.line! "Test 2: $(part2 testText)"
    part2Start = time! {}
    Stdout.line! "Part 2: $(part2 puzzleText)"
    part2End = time! {}
    Stdout.line! "Part 2 time: $(Num.toStr (part2End - part2Start))ms"

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
    findShortestPathToEnd game
    |> Ok

doPart2 : Str -> Result U64 _
doPart2 = \input ->
    game = String.parseStr? gameParser input
    bestScore = findShortestPathToEnd game # calculating again to avoid hard coding the part 1 answer in
    findAllWinningPaths game bestScore
    |> List.walk (Set.empty {}) \soFar, frontier -> Set.union soFar frontier.prevPoints
    |> Set.insert game.start
    |> Set.insert game.end
    |> Set.len
    |> Ok

Game : { start : Point, end : Point, walls : Set Point }
Point : { x : U64, y : U64 }

gameParser : Parser _ Game
gameParser =
    Parser.sepBy lineParser (string "\r\n")
    |> Parser.map makeGame
lineParser =
    Parser.oneOf [
        Parser.const Wall |> skip (string "#"),
        Parser.const Empty |> skip (string "."),
        Parser.const Start |> skip (string "S"),
        Parser.const End |> skip (string "E"),
    ]
    |> Parser.many
makeGame = \lines ->
    List.walkWithIndex lines { start: { x: 0, y: 0 }, end: { x: 0, y: 0 }, walls: Set.empty {} } \soFar, line, lineNum ->
        List.walkWithIndex line soFar \innerSoFar, block, colNum ->
            when block is
                Empty -> innerSoFar
                Start -> { innerSoFar & start: { x: colNum, y: lineNum } }
                End -> { innerSoFar & end: { x: colNum, y: lineNum } }
                Wall -> { innerSoFar & walls: Set.insert innerSoFar.walls { x: colNum, y: lineNum } }

Frontier : { location : Point, direction : Direction, scoreSoFar : U64, prevPoints : Set Point }
Direction : [North, East, South, West]

findShortestPathToEnd = \game ->
    help : List Frontier, Dict (Point, Direction) U64 -> U64
    help = \frontiers, cheapestByPoint ->
        when frontiers is
            [] -> 0
            [cheapest, .. as rest] ->
                if cheapest.location == game.end then
                    cheapest.scoreSoFar
                else
                    ourNextMoves =
                        nextMoves cheapest
                        |> List.dropIf (\move -> Set.contains game.walls move.location)
                        |> List.dropIf (\move -> Set.contains move.prevPoints move.location)
                        |> List.keepIf
                            (\move ->
                                maybeBest = Dict.get cheapestByPoint (move.location, move.direction)
                                when maybeBest is
                                    Err _ -> Bool.true
                                    Ok best -> move.scoreSoFar < best
                            )
                    newCheapestByPoint = List.walk ourNextMoves cheapestByPoint \soFar, move -> Dict.insert soFar (move.location, move.direction) move.scoreSoFar
                    ourNextMoves
                    |> List.concat rest
                    |> List.sortWith (\a, b -> Num.compare a.scoreSoFar b.scoreSoFar)
                    |> help newCheapestByPoint
    help [{ location: game.start, direction: East, scoreSoFar: 0, prevPoints: Set.empty {} }] (Dict.empty {})

nextMoves = \frontier ->
    [goStraight frontier, turnAndStep frontier turnLeft, turnAndStep frontier turnRight]
goStraight = \frontier ->
    { frontier &
        location: stepInDirection frontier.location frontier.direction,
        scoreSoFar: frontier.scoreSoFar + 1,
        prevPoints: Set.insert frontier.prevPoints frontier.location,
    }
turnAndStep = \frontier, turn ->
    newDirection = turn frontier.direction
    {
        location: stepInDirection frontier.location newDirection,
        direction: newDirection,
        scoreSoFar: frontier.scoreSoFar + 1001,
        prevPoints: Set.insert frontier.prevPoints frontier.location,
    }
turnLeft = \dir ->
    when dir is
        North -> West
        East -> North
        South -> East
        West -> South
turnRight = \dir ->
    when dir is
        North -> East
        East -> South
        South -> West
        West -> North
stepInDirection = \point, dir ->
    when dir is
        North -> { point & y: point.y - 1 }
        East -> { point & x: point.x + 1 }
        South -> { point & y: point.y + 1 }
        West -> { point & x: point.x - 1 }

findAllWinningPaths = \game, bestScore ->
    help : List Frontier, Dict (Point, Direction) U64 -> List Frontier
    help = \frontiers, cheapestByPoint ->
        when frontiers is
            [] -> []
            [cheapest, .. as rest] ->
                if cheapest.scoreSoFar == bestScore then
                    frontiers
                else
                    ourNextMoves =
                        nextMoves cheapest
                        |> List.dropIf (\move -> Set.contains game.walls move.location)
                        |> List.dropIf (\move -> Set.contains move.prevPoints move.location)
                        |> List.dropIf (\move -> move.scoreSoFar > bestScore)
                        |> List.keepIf
                            (\move ->
                                maybeBest = Dict.get cheapestByPoint (move.location, move.direction)
                                when maybeBest is
                                    Err _ -> Bool.true
                                    Ok best -> move.scoreSoFar <= best
                            )
                    newCheapestByPoint = List.walk ourNextMoves cheapestByPoint \soFar, move -> Dict.insert soFar (move.location, move.direction) move.scoreSoFar
                    ourNextMoves
                    |> List.concat rest
                    |> List.sortWith (compareFrontiers)
                    |> mergeMatchingLeaders
                    |> help newCheapestByPoint
    help [{ location: game.start, direction: East, scoreSoFar: 0, prevPoints: Set.empty {} }] (Dict.empty {})

compareFrontiers = \a, b ->
    when Num.compare a.scoreSoFar b.scoreSoFar is
        LT -> LT
        GT -> GT
        EQ ->
            when Num.compare a.location.x b.location.x is
                LT -> LT
                GT -> GT
                EQ ->
                    Num.compare a.location.y b.location.y

mergeMatchingLeaders = \frontiers ->
    when frontiers is
        [] | [_] -> frontiers
        [first, second, .. as rest] ->
            if first.location == second.location && first.direction == second.direction && first.scoreSoFar == second.scoreSoFar then
                # dbg (first.location, first.direction, first.scoreSoFar)
                rest
                |> List.prepend { first & prevPoints: Set.union first.prevPoints second.prevPoints }
                |> mergeMatchingLeaders
            else
                frontiers
