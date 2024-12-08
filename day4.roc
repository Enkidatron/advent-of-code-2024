app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br",
}

import pf.Stdout
# import "day4tiny.txt" as day4TestText : Str
import "day4.txt" as day4Text : Str

part1 : Str -> U64
part1 = \text ->
    matrix = cleanAndDivide text
    height = List.len matrix
    width =
        List.map matrix List.len
        |> List.max
        |> Result.withDefault 0
    List.joinMap
        (List.range { start: At 0, end: Before height })
        (\h ->
            List.map
                (List.range { start: At 0, end: Before width })
                (\w -> findScoreFromHere matrix h w))
    |> List.sum

cleanAndDivide : Str -> List (List U8)
cleanAndDivide = \alltext ->
    alltext
    |> Str.splitOn "\r\n"
    |> List.map Str.toUtf8

Direction : [North, South, East, West, NE, SE, SW, NW]

findScoreFromHere : List (List U8), U64, U64 -> U64
findScoreFromHere = \matrix, h, w ->
    [North, South, East, West, NE, SE, SW, NW]
    |> List.keepOks \dir -> getWordFromHere matrix h w dir
    |> List.countIf wordIsXmas

getWordFromHere : List (List U8), U64, U64, Direction -> Result (List U8) [OutOfBounds]
getWordFromHere = \matrix, h, w, dir ->
    signedHeight = Num.toI64Checked? h
    signedWidth = Num.toI64Checked? w
    (hSign, wSign) =
        when dir is
            North -> (-1, 0)
            South -> (1, 0)
            East -> (0, 1)
            West -> (0, -1)
            NW -> (-1, -1)
            NE -> (-1, 1)
            SE -> (1, 1)
            SW -> (1, -1)
    helper : (I64, I64) -> Result U8 [OutOfBounds]
    helper = \(height, width) ->
        List.get? matrix (Num.toU64 height)
            |> List.get (Num.toU64 width)

    [0, 1, 2, 3]
    |> List.map \x -> (signedHeight + (hSign * x), signedWidth + (wSign * x))
    |> List.mapTry helper

wordIsXmas : List U8 -> Bool
wordIsXmas = \word ->
    when word is
        ['X', 'M', 'A', 'S'] -> Bool.true
        _ -> Bool.false

part2 : Str -> U64
part2 = \text ->
    matrix = cleanAndDivide text
    height = List.len matrix
    width =
        List.map matrix List.len
        |> List.max
        |> Result.withDefault 0
    List.joinMap
        (List.range { start: At 0, end: Before height })
        (\h ->
            List.map
                (List.range { start: At 0, end: Before width })
                (\w -> findPart2ScoreFromHere matrix h w))
    |> List.sum

findPart2ScoreFromHere : List (List U8), U64, U64 -> U64
findPart2ScoreFromHere = \matrix, h, w ->
    findCornersFromHere matrix h w
    |> \checkedCorners ->
        when checkedCorners is
            [a, b] if (cornersAreMas a && cornersAreMas b) -> 1
            _ -> 0

findCornersFromHere : List (List U8), U64, U64 -> List (List U8)
findCornersFromHere = \matrix, h, w ->
    [
        [(MinusOne, MinusOne), (Keep, Keep), (PlusOne, PlusOne)],
        [(MinusOne, PlusOne), (Keep, Keep), (PlusOne, MinusOne)],
    ]
        |> List.keepOks
            (\offsets ->
                List.mapTry
                    offsets
                    (\(hOffset, wOffset) ->
                        height = applyOffset? h hOffset
                        width = applyOffset? w wOffset
                        row = List.get? matrix (height)
                        List.get row (width)
                    )

            )
applyOffset : U64, [MinusOne, Keep, PlusOne] -> Result U64 [OutOfBounds]
applyOffset = \num, op ->
    when op is
        MinusOne if num > 0 -> Ok (num - 1)
        MinusOne -> Err OutOfBounds
        Keep -> Ok num
        PlusOne -> Ok (num + 1)

cornersAreMas : List U8 -> Bool
cornersAreMas = \corners ->
    when corners is
        ['M', 'A', 'S'] | ['S', 'A', 'M'] -> Bool.true
        _ -> Bool.false

main =
    # Stdout.line! "Test: $(Num.toStr (part1 day4TestText))"
    Stdout.line! "Part 1: $(Num.toStr (part1 day4Text))"
    Stdout.line! "Part 2: $(Num.toStr (part2 day4Text))"
