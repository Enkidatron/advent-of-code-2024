app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br",
    parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.9.0/w8YKp2YAgQt5REYk912HfKAHBjcXsrnvtjI0CBzoAT4.tar.br",
    # array2d: "https://github.com/mulias/roc-array2d/releases/download/v0.3.1/2Jqajvxn36vRryyQBSluU6Fo6vVI5yNSYmcJcyaKp0Y.tar.br",
}

import pf.Stdout
import pf.Utc
import parser.Parser exposing [Parser, keep, skip]
import parser.String exposing [string]
# import array2d.Array2D
import "day15test.txt" as testText : Str
import "day15.txt" as puzzleText : Str

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
    runGame game
    |> .map
    |> .boxes
    |> Set.toList
    |> List.map calcGPS
    |> List.sum
    |> Ok

doPart2 : Str -> Result U64 _
doPart2 = \input ->
    game = String.parseStr? game2Parser input
    runGame2 game
    |> .map
    |> .boxes
    |> Dict.values
    |> List.map .left
    |> List.map calcGPS
    |> List.sum
    |> Ok

Game : { map : Map, moves : List Move }
Map : { boxes : Set Point, walls : Set Point, bot : Point }
Point : { x : U64, y : U64 }
Move : [Up, Down, Right, Left]

gameParser : Parser _ Game
gameParser =
    Parser.const (\map -> \moves -> { map, moves })
    |> keep (mapParser |> Parser.map makeMap)
    |> skip (string "\r\n")
    |> keep movesParser
mapParser =
    Parser.sepBy mapLineParser (string "\r\n")

mapLineParser =
    Parser.oneOrMore mapTileParser
mapTileParser =
    Parser.oneOf [
        Parser.const Wall |> skip (string "#"),
        Parser.const Box |> skip (string "O"),
        Parser.const Bot |> skip (string "@"),
        Parser.const Empty |> skip (string "."),
    ]
makeMap = \lists ->
    List.walkWithIndex lists { walls: Set.empty {}, boxes: Set.empty {}, bot: { x: 0, y: 0 } } \soFar, line, rowNum ->
        List.walkWithIndex line soFar \innerSoFar, tile, colNum ->
            when tile is
                Wall -> { innerSoFar & walls: Set.insert innerSoFar.walls { x: colNum, y: rowNum } }
                Box -> { innerSoFar & boxes: Set.insert innerSoFar.boxes { x: colNum, y: rowNum } }
                Bot -> { innerSoFar & bot: { x: colNum, y: rowNum } }
                Empty -> innerSoFar

movesParser =
    Parser.sepBy moveRowParser (string "\r\n")
    |> Parser.map List.join
moveRowParser =
    Parser.many moveParser
moveParser =
    Parser.oneOf [
        Parser.const Up |> skip (string "^"),
        Parser.const Down |> skip (string "v"),
        Parser.const Right |> skip (string ">"),
        Parser.const Left |> skip (string "<"),
    ]

runGame : Game -> Game
runGame = \game ->
    when game.moves is
        [] -> game
        [nextMove, .. as rest] ->
            map = game.map
            when findNextSpots game.map (nextMove) is
                MoveBot newBot -> runGame { game & map: { map & bot: newBot }, moves: rest }
                MoveBox newBox newBot -> runGame { game & map: { map & bot: newBot, boxes: game.map.boxes |> Set.remove newBot |> Set.insert newBox }, moves: rest }
                Blocked -> runGame { game & moves: rest }

findNextSpots = \map, move ->
    offset = getOffset move

    nextBotPoint = offset map.bot
    if Set.contains map.walls (nextBotPoint) then
        Blocked
    else if Set.contains map.boxes (nextBotPoint) then
        help = \boxPoint ->
            nextBoxPoint = offset boxPoint
            if Set.contains map.walls (nextBoxPoint) then
                Blocked
            else if Set.contains map.boxes (nextBoxPoint) then
                help nextBoxPoint
            else
                MoveBox nextBoxPoint nextBotPoint
        help nextBotPoint
    else
        MoveBot nextBotPoint

getOffset = \move ->
    when move is
        Up -> \p -> { p & y: p.y - 1 }
        Down -> \p -> { p & y: p.y + 1 }
        Right -> \p -> { p & x: p.x + 1 }
        Left -> \p -> { p & x: p.x - 1 }
calcGPS : Point -> U64
calcGPS = \{ x, y } ->
    (y * 100) + x

# Part 2

Game2 : { map : Map2, moves : List Move }
Map2 : { boxes : Dict U64 Box2, walls : Set Point, bot : Point }
Box2 : { left : Point, right : Point }

game2Parser : Parser _ Game2
game2Parser =
    Parser.const (\map -> \moves -> { map, moves })
    |> keep (mapParser |> Parser.map makeMap2)
    |> skip (string "\r\n")
    |> keep movesParser

makeMap2 = \lists ->
    intermediate = List.walkWithIndex
        lists
        { walls: Set.empty {}, boxes: [], bot: { x: 0, y: 0 } }
        \soFar, line, rowNum ->
            List.walkWithIndex
                line
                soFar
                \innerSoFar, tile, colNum ->
                    when tile is
                        Wall -> { innerSoFar & walls: innerSoFar.walls |> Set.insert { x: colNum * 2, y: rowNum } |> Set.insert { x: (colNum * 2) + 1, y: rowNum } }
                        Box -> { innerSoFar & boxes: innerSoFar.boxes |> List.append { left: { x: colNum * 2, y: rowNum }, right: { x: (colNum * 2) + 1, y: rowNum } } }
                        Bot -> { innerSoFar & bot: { x: colNum * 2, y: rowNum } }
                        Empty -> innerSoFar

    {
        walls: intermediate.walls,
        bot: intermediate.bot,
        boxes: intermediate.boxes
        |> List.mapWithIndex \box, index -> (index, box)
        |> Dict.fromList,
    }

runGame2 : Game2 -> Game2
runGame2 = \game ->
    when game.moves is
        [] -> game
        [nextMove, .. as rest] ->
            map = game.map
            when findNextSpots2 game.map (nextMove) is
                MoveBot newBot affectedBoxes ->
                    runGame2
                        { game &
                            map: { map &
                                bot: newBot,
                                boxes: map.boxes
                                |> Dict.map \boxId, box ->
                                    if Set.contains affectedBoxes boxId then
                                        moveBox2 box nextMove
                                    else
                                        box,
                            },
                            moves: rest,
                        }

                Blocked -> runGame2 { game & moves: rest }
moveBox2 = \box, move ->
    offset = getOffset move
    { left: offset box.left, right: offset box.right }

findNextSpots2 = \map, move ->
    offset = getOffset move
    pointsWithBoxes = Dict.walk map.boxes (Dict.empty {}) \soFar, boxId, box ->
        soFar |> Dict.insert box.left boxId |> Dict.insert box.right boxId
    nextBotPoint = offset map.bot
    help : List Point, Set U64 -> [Blocked, MoveBot Point (Set U64)]
    help = \nextPoints, idsSoFar ->
        if List.any nextPoints \point -> Set.contains map.walls point then
            Blocked
        else
            impactedBoxIds =
                List.keepOks nextPoints (\point -> Dict.get pointsWithBoxes point)
                |> List.dropIf \boxId -> Set.contains idsSoFar boxId
            when impactedBoxIds is
                [] -> MoveBot nextBotPoint idsSoFar
                _ ->
                    newIds = Set.union idsSoFar (Set.fromList impactedBoxIds)
                    newPoints =
                        impactedBoxIds
                        |> List.keepOks (\boxId -> Dict.get map.boxes boxId)
                        |> List.joinMap (\box -> [offset box.left, offset box.right])
                    help newPoints newIds

    help [nextBotPoint] (Set.empty {})
