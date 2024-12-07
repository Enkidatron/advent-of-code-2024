app [main] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br" 
    , parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.9.0/w8YKp2YAgQt5REYk912HfKAHBjcXsrnvtjI0CBzoAT4.tar.br",
}

import pf.Stdout
import parser.Parser exposing [ skip]
import parser.String exposing [string]
import "day6test.txt" as testText : Str 
import "day6.txt" as puzzleText : Str 

main =
    # Stdout.line! "Test 1: $(part1 testText)"    
    # Stdout.line! "Part 1: $(part1 puzzleText)"
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

doPart1 : Str -> Result U64 [NotDoneYet, ParsingFailure Str, ParsingIncomplete Str, NotFound, OutOfBounds]
doPart1 = \input -> 
    board = parseBoard? input 
    game = gameFromBoard? board 
    finishedGame = stepUntilDone game
    finishedGame.visited |> Set.len |> Ok  

expect doPart1 testText == Ok 41

doPart2 : Str -> Result U64 [NotDoneYet, ParsingFailure Str, ParsingIncomplete Str, NotFound, OutOfBounds]
doPart2 = \input -> 
    board = parseBoard? input 
    startingGame = gameFromBoard? board
    defaultOutcome = stepUntilDone startingGame 
    defaultOutcome.visited 
        |> Set.toList 
        |> List.countIf (\point -> 
            if point == startingGame.guardCoords then 
                Bool.false 
            else 
                { startingGame & obstacles: Set.insert startingGame.obstacles point }
                    |> stepUntilDone  
                    |> guardIsOutside 
                    |> Bool.not  
        )
        |> Ok 

Direction : [Up, Down, Left, Right]
Point : { line : I64, col : I64}
DirectedPoint : { line : I64, col : I64, dir : Direction }
Board : List (List [Empty, Visited, Obstacle, Guard Direction])
Game : { guardCoords : Point, guardDir : Direction, obstacles : Set Point, visited : Set Point, boardHeight : I64, boardWidth : I64
    , prevPositions : Set DirectedPoint }

parseBoard = \input -> 
    String.parseStr boardParser input 

boardParser = 
    Parser.sepBy lineParser (string "\r\n")

lineParser = 
    Parser.oneOf 
        [ Parser.const Empty |> skip (string ".")
        , Parser.const Obstacle |> skip (string "#")
        , Parser.const (Guard Up) |> skip (string "^")
        ]
        |> Parser.many 

gameFromBoard : Board -> Result Game [NotFound, OutOfBounds ] 
gameFromBoard = \board -> 
    guardLine = List.findFirstIndex? board (\line -> List.any line (\block -> block == (Guard Up)))
    guardCol = List.get board guardLine 
        |> Result.try? (\line -> List.findFirstIndex line (\block -> block == (Guard Up)))
    boardWidth = List.get board 0 
        |> Result.map List.len
        |> Result.map? Num.toI64 
    obstacles : Set Point
    obstacles = board 
        |> List.walkWithIndex (Set.empty {}) (\soFar, line, lineNum -> 
            List.walkWithIndex line soFar (\innerSoFar, block, colNum -> 
                when block is 
                    Obstacle -> Set.insert innerSoFar { line : Num.toI64 lineNum, col : Num.toI64 colNum }  
                    _ -> innerSoFar 
            )
        )
    Ok ({ guardCoords : { line : Num.toI64 guardLine, col : Num.toI64 guardCol} , guardDir : Up 
        , obstacles
        , visited : Set.empty {}
        , prevPositions : Set.empty {} 
        , boardHeight : List.len board |> Num.toI64
        , boardWidth 
    })
    
stepUntilDone : Game -> Game
stepUntilDone = \game -> 
    when step game is 
        Continue newGame -> stepUntilDone newGame 
        Break newGame -> newGame 
step : Game -> [Continue Game, Break Game ]
step = \game -> 
    if guardIsOutside game then 
        Break game 
    else 
        nextPoint = getNextPoint game.guardCoords game.guardDir 
        currentDirectedPoint = { line: game.guardCoords.line, col: game.guardCoords.col, dir: game.guardDir }
        if Set.contains game.prevPositions currentDirectedPoint then 
            Break game 
        else if Set.contains game.obstacles nextPoint then 
            Continue { game & guardDir: rotateClockwise game.guardDir }
        else 
            Continue { game & guardCoords: nextPoint, visited: Set.insert game.visited game.guardCoords, prevPositions: Set.insert game.prevPositions currentDirectedPoint }

guardIsOutside : Game -> Bool 
guardIsOutside = \game -> 
    game.guardCoords.line < 0 || game.guardCoords.line >= game.boardHeight || game.guardCoords.col < 0 || game.guardCoords.col >= game.boardWidth

getNextPoint : Point, Direction -> Point 
getNextPoint = \point, dir -> 
    when dir is 
        Up -> { point & line: point.line - 1}
        Down -> { point & line: point.line + 1}
        Right -> { point & col: point.col + 1}
        Left -> { point & col: point.col - 1}

rotateClockwise : Direction -> Direction
rotateClockwise = \dir -> 
    when dir is 
        Up -> Right 
        Right -> Down 
        Down -> Left 
        Left -> Up 