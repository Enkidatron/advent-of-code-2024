app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br",
    parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.9.0/w8YKp2YAgQt5REYk912HfKAHBjcXsrnvtjI0CBzoAT4.tar.br",
    # array2d: "https://github.com/mulias/roc-array2d/releases/download/v0.3.1/2Jqajvxn36vRryyQBSluU6Fo6vVI5yNSYmcJcyaKp0Y.tar.br",
}

import pf.Stdout
import pf.Utc
import parser.Parser exposing [Parser, keep, skip, const]
import parser.String exposing [string]
# import array2d.Array2D
import "day19test.txt" as testText : Str
import "day19.txt" as puzzleText : Str

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
        Ok answer -> Num.toStr answer
        Err _ -> "Error"

part2 : Str -> Str
part2 = \input ->
    when doPart2 input is
        Ok answer -> Num.toStr answer
        Err _ -> "Error"

doPart1 : Str -> Result U64 _
doPart1 = \input ->
    onsen = String.parseStr? onsenParser input
    # List.countIf onsen.desired \desired -> canMakeFromPatterns desired onsen.available
    List.map onsen.desired \desired -> countArrangements desired onsen.available
    |> List.countIf \arrangements -> arrangements > 0
    |> Ok

doPart2 : Str -> Result U64 _
doPart2 = \input ->
    onsen = String.parseStr? onsenParser input
    List.map onsen.desired \desired -> countArrangements desired onsen.available
    |> List.sum
    |> Ok

Onsen : { available : List Pattern, desired : List Pattern }
Pattern : List Stripe
Stripe : [White, Blue, Black, Red, Green]
onsenParser : Parser _ Onsen
onsenParser =
    Parser.const (\available -> \desired -> { available, desired })
    |> keep availableParser
    |> skip (string "\r\n\r\n")
    |> keep desiredParser

patternParser =
    [
        const White |> skip (string "w"),
        const Blue |> skip (string "u"),
        const Black |> skip (string "b"),
        const Red |> skip (string "r"),
        const Green |> skip (string "g"),
    ]
    |> Parser.oneOf
    |> Parser.many

availableParser =
    patternParser
    |> Parser.sepBy (string ", ")
desiredParser =
    patternParser
    |> Parser.sepBy (string "\r\n")

canMakeFromPatterns : Pattern, List Pattern -> Bool
canMakeFromPatterns = \desired, patterns ->
    setOfPatterns = Set.fromList patterns
    continuePattern = \desiredStripe, pattern ->
        when pattern is
            [patternStripe, .. as rest] ->
                if patternStripe == desiredStripe then
                    Set.single (InPattern rest)
                else
                    Set.empty {}

            _ -> Set.empty {}
    continue = \desiredStripe, continuation ->
        when continuation is
            Fresh ->
                setOfPatterns |> Set.joinMap \pattern -> continuePattern desiredStripe pattern

            InPattern pattern ->
                continuePattern desiredStripe pattern
    List.walk
        desired
        (Set.single Fresh)
        (\continuations, desiredStripe ->
            continuations
            |> Set.joinMap (\continuation -> continue desiredStripe continuation)
            |> Set.map freshen
        )
    |> Set.contains Fresh

freshen = \c ->
    when c is
        InPattern [] -> Fresh
        _ -> c

Continuation : [Fresh, InPattern Pattern]

countArrangements : Pattern, List Pattern -> U64
countArrangements = \desired, patterns ->
    step : Dict Continuation U64, Stripe -> Dict Continuation U64
    step = \continuations, desiredStripe ->
        addNextContinuation : Dict Continuation U64, Continuation, U64 -> Dict Continuation U64
        addNextContinuation = \soFar, continuation, count ->
            continuePattern : Dict Continuation U64, Pattern -> Dict Continuation U64
            continuePattern = \innerSoFar, pattern ->
                when pattern is
                    [patternStripe, .. as rest] ->
                        if patternStripe == desiredStripe then
                            newContinuation = freshen (InPattern rest)
                            insertCount innerSoFar newContinuation count
                        else
                            innerSoFar

                    _ -> innerSoFar

            when continuation is
                Fresh -> List.walk patterns soFar continuePattern
                InPattern pattern ->
                    continuePattern soFar pattern
        Dict.walk continuations (Dict.empty {}) addNextContinuation
    List.walk desired (Dict.single Fresh 1) step
    |> Dict.get Fresh
    |> Result.withDefault 0

insertCount = \dict, key, count ->
    Dict.update dict key \val ->
        when val is
            Ok num -> Ok (num + count)
            Err _ -> Ok count
