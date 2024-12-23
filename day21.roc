app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br",
    parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.9.0/w8YKp2YAgQt5REYk912HfKAHBjcXsrnvtjI0CBzoAT4.tar.br",
    # array2d: "https://github.com/mulias/roc-array2d/releases/download/v0.3.1/2Jqajvxn36vRryyQBSluU6Fo6vVI5yNSYmcJcyaKp0Y.tar.br",
}

import pf.Stdout
import pf.Utc
import parser.Parser exposing [Parser, skip, keep, const]
import parser.String exposing [string, digit]
# import array2d.Array2D
import "day21test.txt" as testText : Str
import "day21.txt" as puzzleText : Str

time = \{} -> Utc.now {} |> Task.map Utc.toMillisSinceEpoch
main =
    # Stdout.line! "Test 1: $(part1 testText)"
    part1Start = time! {}
    Stdout.line! "Part 1: $(part1 puzzleText)"
    part1End = time! {}
    Stdout.line! "Part 1 time: $(Num.toStr (part1End - part1Start))ms"
    # Stdout.line! "Test 2: $(part2 testText)"
    part2Start = time! {}
    Stdout.line! "Part 2: $(part2 puzzleText)"
    part2End = time! {}
    Stdout.line! "Part 2 time: $(Num.toStr (part2End - part2Start))ms"

part1 : Str -> Str
part1 = \input ->
    when doPart1 input is
        Ok answer -> Num.toStr answer
        Err _ -> "Error"

part2 : Str -> Str
part2 = \input ->
    when doPart2 input is
        Ok answer -> Num.toStr answer
        Err _ -> "Error"

doPart1 : Str -> Result U64 _
doPart1 = \input ->
    codes = String.parseStr? codesParser input
    # _ = dbg shortestLevel1Sequences
    # _ = dbg shortestLevel2Sequences
    dict = shortestLevelNSequences 2
    # _ = dbg (shortestLevelNSequences 2 == shortestLevel2Sequences)

    codes
    |> List.map \code -> (code, code |> calcShortestSequenceUsingDict dict)
    |> List.map calcComplexity
    |> List.sum
    |> Ok

doPart2 : Str -> Result U64 _
doPart2 = \input ->
    codes = String.parseStr? codesParser input
    dict = shortestLevelNSequences 25
    codes
    |> List.map \code -> (code, calcShortestSequenceUsingDict code dict)
    |> List.map calcComplexity
    |> List.sum
    |> Ok

Code : List NumKeyPad
NumKeyPad : [Digit U8, Activate]
DirKeyPad : [Left, Right, Up, Down, Activate]
codesParser : Parser _ (List Code)
codesParser =
    Parser.oneOf [
        const Digit |> keep (digit |> Parser.map Num.toU8),
        const Activate |> skip (string "A"),
    ]
    |> Parser.many
    |> Parser.sepBy (string "\r\n")

allDirKeys = [Left, Right, Up, Down, Activate]
shortestLevel1Sequences : Dict (DirKeyPad, DirKeyPad) U64
shortestLevel1Sequences =
    List.walk allDirKeys (Dict.empty {}) \outerSoFar, curr ->
        List.walk allDirKeys outerSoFar \soFar, next ->
            options = dirKeyOptionsForDirKey curr next
            bestLength = options |> List.map List.len |> List.min
            when bestLength is
                Ok length -> Dict.insert soFar (curr, next) length
                Err _ -> crash "bad level 1"
findLevel2Length : List DirKeyPad -> U64
findLevel2Length = \keys ->
    when keys is
        [] -> crash "empty level 2 dir keys"
        _ ->
            List.walk keys (Activate, 0) \(curr, soFar), next ->
                bestMaybe = Dict.get shortestLevel1Sequences (curr, next)
                when bestMaybe is
                    Ok best -> (next, best + soFar)
                    Err _ -> crash "cannot find best level 1 sequence in level 2"
            |> .1
shortestLevel2Sequences : Dict (DirKeyPad, DirKeyPad) U64
shortestLevel2Sequences =
    List.walk allDirKeys (Dict.empty {}) \outerSoFar, curr ->
        List.walk allDirKeys outerSoFar \soFar, next ->
            options = dirKeyOptionsForDirKey curr next
            bestLength = options |> List.map findLevel2Length |> List.min
            when bestLength is
                Ok best -> Dict.insert soFar (curr, next) best
                Err _ -> crash "no options when finding level 2 sequences"

findLevel3Length : List DirKeyPad -> U64
findLevel3Length = \keys ->
    when keys is
        [] -> crash "empty level 3 keys when finding length"
        _ ->
            length =
                List.walk keys (Activate, 0) \(curr, soFar), next ->
                    bestMaybe = Dict.get shortestLevel2Sequences (curr, next)
                    when bestMaybe is
                        Ok best -> (next, best + soFar)
                        Err _ -> crash "cannot find best level 2 sequence in level 3"
                |> .1
            length
findSequenceLengthUsingDict : List DirKeyPad, Dict (DirKeyPad, DirKeyPad) U64 -> U64
findSequenceLengthUsingDict = \keys, dict ->
    when keys is
        [] -> crash "empty level N dir keys"
        _ ->
            List.walk keys (Activate, 0) \(curr, soFar), next ->
                bestMaybe = Dict.get dict (curr, next)
                when bestMaybe is
                    Ok best -> (next, best + soFar)
                    Err _ -> crash "cannot find best level N-1 sequence in level N"
            |> .1
shortestLevelNSequences : U64 -> Dict (DirKeyPad, DirKeyPad) U64
shortestLevelNSequences = \level ->
    List.range { start: After 1, end: At level }
    |> List.walk shortestLevel1Sequences \lastLevel, _ ->
        List.walk allDirKeys (Dict.empty {}) \outerSoFar, curr ->
            List.walk allDirKeys outerSoFar \soFar, next ->
                options = dirKeyOptionsForDirKey curr next
                bestLength =
                    options
                    |> List.map \opt -> findSequenceLengthUsingDict opt lastLevel
                    |> List.min
                when bestLength is
                    Ok best -> Dict.insert soFar (curr, next) best
                    Err _ -> crash "no options when finding level N sequences"
calcShortestSequence : Code -> U64
calcShortestSequence = \code ->
    step : (NumKeyPad, U64), NumKeyPad -> (NumKeyPad, U64)
    step = \(curr, soFar), next ->
        firstRobotOptions = dirKeyOptionsForNumKey curr next
        bestMaybe =
            List.map firstRobotOptions findLevel3Length
            |> List.min
        when bestMaybe is
            Ok best -> (next, best + soFar)
            Err _ -> crash "cannot find best level 3 sequence"
    List.walk code (Activate, 0) step
    |> .1
calcShortestSequenceUsingDict : Code, Dict (DirKeyPad, DirKeyPad) U64 -> U64
calcShortestSequenceUsingDict = \code, dict ->
    step : (NumKeyPad, U64), NumKeyPad -> (NumKeyPad, U64)
    step = \(curr, soFar), next ->
        firstRobotOptions = dirKeyOptionsForNumKey curr next
        bestMaybe =
            List.map firstRobotOptions (\opt -> findSequenceLengthUsingDict opt dict)
            |> List.min
        when bestMaybe is
            Ok best -> (next, best + soFar)
            Err _ -> crash "cannot find best level N sequence"
    List.walk code (Activate, 0) step
    |> .1

getNumKeyX : NumKeyPad -> I64
getNumKeyX = \keyPad ->
    when keyPad is
        Digit 7 | Digit 4 | Digit 1 -> 0
        Digit 8 | Digit 5 | Digit 2 | Digit 0 -> 1
        Digit 9 | Digit 6 | Digit 3 | Activate -> 2
        _ -> crash "bad NumKeyPad"
getNumKeyY : NumKeyPad -> I64
getNumKeyY = \keyPad ->
    when keyPad is
        Digit 7 | Digit 8 | Digit 9 -> 0
        Digit 4 | Digit 5 | Digit 6 -> 1
        Digit 1 | Digit 2 | Digit 3 -> 2
        Digit 0 | Activate -> 3
        _ -> crash "bad NumKeyPad"

dirKeyOptionsForNumKey : NumKeyPad, NumKeyPad -> List (List DirKeyPad)
dirKeyOptionsForNumKey = \curr, next ->
    currX = getNumKeyX curr
    currY = getNumKeyY curr
    nextX = getNumKeyX next
    nextY = getNumKeyY next
    dx = nextX - currX
    dy = nextY - currY
    leftRight = if dx < 0 then List.repeat Left (Num.abs dx |> Num.toU64) else List.repeat Right (Num.toU64 dx)
    upDown = if dy < 0 then List.repeat Up (Num.abs dy |> Num.toU64) else List.repeat Down (Num.toU64 dy)
    if ((curr == Activate && dx == -2) || (curr == (Digit 0) && dx == -1)) then
        [List.join [upDown, leftRight, [Activate]]]
    else if ((next == Activate && dx == 2) || (next == (Digit 0) && dx == 1)) then
        [List.join [leftRight, upDown, [Activate]]]
    else
        [List.join [upDown, leftRight, [Activate]], List.join [leftRight, upDown, [Activate]]]

getDirKeyX : DirKeyPad -> I64
getDirKeyX = \keyPad ->
    when keyPad is
        Left -> 0
        Up | Down -> 1
        Right | Activate -> 2
getDirKeyY : DirKeyPad -> I64
getDirKeyY = \keyPad ->
    when keyPad is
        Up | Activate -> 0
        Left | Down | Right -> 1
dirKeyOptionsForDirKey : DirKeyPad, DirKeyPad -> List (List DirKeyPad)
dirKeyOptionsForDirKey = \curr, next ->
    currX = getDirKeyX curr
    currY = getDirKeyY curr
    nextX = getDirKeyX next
    nextY = getDirKeyY next
    dx = nextX - currX
    dy = nextY - currY
    leftRight = if dx < 0 then List.repeat Left (Num.abs dx |> Num.toU64) else List.repeat Right (Num.toU64 dx)
    upDown = if dy < 0 then List.repeat Up (Num.abs dy |> Num.toU64) else List.repeat Down (Num.toU64 dy)
    if ((curr == Activate && dx == -2) || (curr == Up && dx == -1)) then
        [List.join [upDown, leftRight, [Activate]]]
    else if ((next == Activate && dx == 2) || (next == Up && dx == 1)) then
        [List.join [leftRight, upDown, [Activate]]]
    else
        [List.join [upDown, leftRight, [Activate]], List.join [leftRight, upDown, [Activate]]]

calcComplexity : (Code, U64) -> U64
calcComplexity = \(code, keyLength) ->
    codeNumR =
        code
        |> List.map
            (\k ->
                when k is
                    Digit n -> Num.toStr n
                    Activate -> ""
            )
        |> Str.joinWith ""
        |> Str.toU64
    codeNum =
        when codeNumR is
            Ok num -> num
            Err _ -> crash "bad code"
    dbg keyLength

    dbg codeNum

    keyLength * codeNum
