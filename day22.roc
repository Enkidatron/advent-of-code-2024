app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br",
    parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.9.0/w8YKp2YAgQt5REYk912HfKAHBjcXsrnvtjI0CBzoAT4.tar.br",
    # array2d: "https://github.com/mulias/roc-array2d/releases/download/v0.3.1/2Jqajvxn36vRryyQBSluU6Fo6vVI5yNSYmcJcyaKp0Y.tar.br",
}

import pf.Stdout
import pf.Utc
import parser.Parser exposing [Parser]
import parser.String exposing [string, digits]
# import array2d.Array2D
import "day22test.txt" as testText : Str
import "day22.txt" as puzzleText : Str

time = \{} -> Utc.now {} |> Task.map Utc.toMillisSinceEpoch
main =
    Stdout.line! "Test 1: $(part1 testText)"
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

doPart1 : Str -> Result I64 _
doPart1 = \input ->
    initialSecrets = String.parseStr? initialSecretsParser input
    initialSecrets
    |> List.map (\secret -> advanceN secret 2000)
    |> List.sum
    |> Ok

doPart2 : Str -> Result I64 _
doPart2 = \input ->
    initialSecrets = String.parseStr? initialSecretsParser input

    initialSecrets
    |> List.map toSeqDict
    |> List.walk (Dict.empty {}) \outerSoFar, seqDict ->
        Dict.walk seqDict outerSoFar \soFar, seq, price ->
            Dict.update soFar seq \maybeVal ->
                when maybeVal is
                    Ok total -> Ok (total + price)
                    Err _ -> Ok price
    |> Dict.values
    |> List.max

initialSecretsParser : Parser _ (List I64)
initialSecretsParser =
    digits |> Parser.map Num.toI64 |> Parser.sepBy (string "\r\n")

advanceN : I64, I64 -> I64
advanceN = \secret, n ->
    if n <= 0 then
        secret
    else
        secret
        |> advance
        |> advanceN (n - 1)
advance : I64 -> I64
advance = \secret ->
    secret
    |> mix (\s -> s * 64)
    |> prune
    |> mix (\s -> s // 32)
    |> prune
    |> mix (\s -> s * 2048)
    |> prune
mix : I64, (I64 -> I64) -> I64
mix = \s, fun ->
    Num.bitwiseXor s (fun s)

prune : I64 -> I64
prune = \s ->
    Num.rem s 16777216

Sequence : (I64, I64, I64, I64)

toSeqDict : I64 -> Dict Sequence I64
toSeqDict = \secret ->
    p0 = priceOf secret
    s1 = advance secret
    p1 = priceOf s1
    w = p1 - p0
    s2 = advance s1
    p2 = priceOf s2
    x = p2 - p1
    s3 = advance s2
    p3 = priceOf s3
    y = p3 - p2
    toSeqDictHelp s3 (Dict.empty {}) w x y p3 1997

toSeqDictHelp : I64, Dict Sequence I64, I64, I64, I64, I64, I64 -> Dict Sequence I64
toSeqDictHelp = \secret, dict, w, x, y, lastPrice, ticksLeft ->
    if ticksLeft == 0 then
        dict
    else
        newSecret = advance secret
        price = priceOf newSecret
        change = price - lastPrice
        newDict = Dict.update dict (w, x, y, change) \val ->
            when val is
                Ok p -> Ok p
                Err _ -> Ok price
        toSeqDictHelp newSecret newDict x y change price (ticksLeft - 1)

priceOf : I64 -> I64
priceOf = \s -> Num.rem s 10
