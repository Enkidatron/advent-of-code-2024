app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br",
}

import pf.Stdout
import pf.Utc
import "day11test.txt" as testText : Str
import "day11.txt" as puzzleText : Str

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
    stones = input |> Str.splitOn " " |> List.keepOks Str.toU64
    stonesDict = stones |> List.map (\s -> (s, 1)) |> Dict.fromList
    List.repeat {} 25
    |> List.walk
        stonesDict
        (\soFar, _ ->
            blinkDict soFar)
    |> Dict.values
    |> List.sum
    |> Ok

doPart2 : Str -> Result U64 _
doPart2 = \input ->
    stones = input |> Str.splitOn " " |> List.keepOks Str.toU64
    stonesDict = stones |> List.map (\s -> (s, 1)) |> Dict.fromList
    List.repeat {} 75
    |> List.walk
        stonesDict
        (\soFar, _ ->
            blinkDict soFar)
    |> Dict.values
    |> List.sum
    |> Ok

blinkDict = \stones ->
    Dict.walk stones (Dict.empty {}) \soFar, stone, count ->
        when stone is
            0 -> addCountToDict soFar 1 count # our X '0' stones become X '1' stones
            _ ->
                when splitDigits stone is
                    Ok (left, right) ->
                        soFar
                        |> addCountToDict left count
                        |> addCountToDict right count # our X stones become X left stones and X right stones

                    Err _ ->
                        addCountToDict soFar (stone * 2024) count # our X stones becomes X multiplied stones

addCountToDict = \dict, key, numToAdd ->
    Dict.update dict key \current ->
        when current is
            Ok num -> Ok (num + numToAdd)
            Err _ -> Ok numToAdd

splitDigits = \stone ->
    utf8 = stone |> Num.toStr |> Str.toUtf8
    numChars = List.len utf8
    if Num.isEven (numChars) then
        { before, others } = List.splitAt utf8 (numChars // 2)
        leftText = before |> Str.fromUtf8?
        left = Str.toU64? leftText
        rightText = others |> Str.fromUtf8?
        right = Str.toU64? rightText
        Ok (left, right)
    else
        Err OddDigits
