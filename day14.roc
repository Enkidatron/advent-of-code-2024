app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br",
    parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.9.0/w8YKp2YAgQt5REYk912HfKAHBjcXsrnvtjI0CBzoAT4.tar.br",
}

import pf.Stdout
import pf.Stdin
import pf.Utc
import parser.Parser exposing [Parser, keep, skip]
import parser.String exposing [digits, string]
import "day14test.txt" as testText : Str
import "day14.txt" as puzzleText : Str

time = \{} -> Utc.now {} |> Task.map Utc.toMillisSinceEpoch
main =
    Stdout.line! "Test 1: $(part1 testText TestArena)"
    part1Start = time! {}
    Stdout.line! "Part 1: $(part1 puzzleText RealArena)"
    part1End = time! {}
    Stdout.line! "Part 1 time: $(Num.toStr (part1End - part1Start))ms"
    # Stdout.line! "Test 2: $(part2 testText)"
    # part2Start = time! {}
    # Stdout.line! "Part 2: $(part2 puzzleText)"
    # part2End = time! {}
    # Stdout.line! "Part 2 time: $(Num.toStr (part2End - part2Start))ms"
    _ = part2Interactive!
    Stdout.line! "goodbye"

part1 : Str, Arena -> Str
part1 = \input, arena ->
    when doPart1 input arena is
        Ok num -> Num.toStr num
        Err _ -> "Error"

doPart1 : Str, Arena -> Result U64 _
doPart1 = \input, arena ->
    robots = String.parseStr? robotsParser input
    endPositions = List.map
        robots
        (\robot -> robot.velocity
            |> scalarMultiply 100
            |> addVectors robot.position
            |> normalizeToArena arena
        )
    (xDivider, yDivider) =
        when arena is
            TestArena -> (5, 3)
            RealArena -> (50, 51)
    List.walk
        endPositions
        { nw: 0, ne: 0, sw: 0, se: 0 }
        (\soFar, robot ->
            when (Num.compare robot.x xDivider, Num.compare robot.y yDivider) is
                (LT, LT) -> { soFar & nw: soFar.nw + 1 }
                (LT, GT) -> { soFar & sw: soFar.sw + 1 }
                (GT, LT) -> { soFar & ne: soFar.ne + 1 }
                (GT, GT) -> { soFar & se: soFar.se + 1 }
                _ -> soFar
        )
    |> \q -> q.nw * q.ne * q.sw * q.se
    |> Ok

part2Interactive =
    robots = String.parseStr robotsParser puzzleText |> Task.fromResult!
    Task.loop (0, robots) \(tick, robotsNow) ->
        _ = displayRobots! robotsNow
        Stdout.line! (Num.toStr tick)
        input = Stdin.line!
        when input is
            "q" -> Task.ok (Done tick)
            chars ->
                num = Str.toU64 chars |> Result.withDefault 1
                newTick = tick + num
                newRobots = List.repeat {} num |> List.walk robotsNow \robotsSoFar, _ -> List.map robotsSoFar tickBot
                Task.ok (Step (newTick, newRobots))

Arena : [TestArena, RealArena]
Vector : { x : I64, y : I64 }
Robot : { position : Vector, velocity : Vector }
robotsParser : Parser _ (List Robot)
robotsParser =
    Parser.sepBy robotParser (string "\r\n")
robotParser : Parser _ Robot
robotParser =
    Parser.const (\position -> \velocity -> { position, velocity })
    |> skip (string "p=")
    |> keep vectorParser
    |> skip (string " v=")
    |> keep vectorParser
vectorParser =
    Parser.const (\x -> \y -> { x, y })
    |> keep integerParser
    |> skip (string ",")
    |> keep integerParser
integerParser =
    Parser.oneOf [
        digits |> Parser.map Num.toI64,
        Parser.const (\num -> Num.toI64 num * -1)
        |> Parser.skip (string "-")
        |> Parser.keep digits,
    ]
scalarMultiply = \vec, scalar ->
    { x: vec.x * scalar, y: vec.y * scalar }
addVectors = \v, u ->
    { x: v.x + u.x, y: v.y + u.y }

normalizeToArena = \vec, arena ->
    (width, height) =
        when arena is
            TestArena -> (11, 7)
            RealArena -> (101, 103)
    { x: vec.x |> mod width, y: vec.y |> mod height }

mod = \a, b ->
    ((a % b) + b) % b

tickBot = \robot ->
    { robot &
        position: robot.position |> addVectors robot.velocity |> normalizeToArena RealArena,
    }
displayRobots = \robots ->
    robotPositions = robots |> List.map .position |> Set.fromList
    List.range { start: At 0, end: Before 103 }
    |> List.map
        (\x ->
            rowText =
                List.range { start: At 0, end: Before 101 }
                |> List.map (\y -> if Set.contains robotPositions { x, y } then "#" else ".")
                |> Str.joinWith ""
            Stdout.line rowText
        )
    |> Task.sequence
