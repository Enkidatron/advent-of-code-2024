app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br",
    parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.9.0/w8YKp2YAgQt5REYk912HfKAHBjcXsrnvtjI0CBzoAT4.tar.br",
}

import pf.Stdout
import "day3.txt" as day3Text : Str
import parser.Parser exposing [Parser]
import parser.String exposing [string, digits]

mulParser : Parser _ U64
mulParser =
    Parser.const (\x -> \y -> x * y)
    |> Parser.skip (string "mul(")
    |> Parser.keep (digits)
    |> Parser.skip (string ",")
    |> Parser.keep (digits)
    |> Parser.skip (string ")")
garbageParser : Parser _ U8
garbageParser =
    String.codeunitSatisfies (\_ -> Bool.true)
doParser =
    Parser.const Do |> Parser.skip (string "do()")
dontParser =
    Parser.const Dont |> Parser.skip (string "don't()")

eitherParser : Parser _ (Result U64 U8)
eitherParser =
    Parser.oneOf [Parser.map mulParser Ok, Parser.map garbageParser Err]

listParser =
    Parser.many eitherParser
    |> Parser.map (\list -> List.keepOks list (\a -> a))

part1Answer : Str
part1Answer =
    String.parseStr listParser day3Text
    |> Result.map List.sum
    |> Result.map Num.toStr
    |> Result.withDefault "error"

part2MultiParser =
    Parser.oneOf [
        Parser.map mulParser Mul,
        doParser,
        dontParser,
        Parser.map garbageParser (\_ -> Garbage),
    ]

part2ListParser =
    Parser.many part2MultiParser
    |> Parser.map (\list -> List.walk list (Do, 0) step)

step : ([Do, Dont], U64), [Mul U64, Do, Dont, Garbage] -> ([Do, Dont], U64)
step = \(thereIsNoTry, sumSoFar), next ->
    when (thereIsNoTry, next) is
        (Do, Mul num) ->
            (Do, sumSoFar + num)

        (Dont, Mul _) ->
            (Dont, sumSoFar)

        (_, Garbage) ->
            (thereIsNoTry, sumSoFar)

        (_, Do) ->
            (Do, sumSoFar)

        (_, Dont) ->
            (Dont, sumSoFar)

# part2TestText = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"
part2Answer =
    String.parseStr part2ListParser day3Text
    |> Result.map .1
    |> Result.map Num.toStr
    |> Result.withDefault "error"

main =
    Stdout.line! "Part 1: $(part1Answer)"
    Stdout.line! "Part 2: $(part2Answer)"
