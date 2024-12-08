app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br",
    parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.9.0/w8YKp2YAgQt5REYk912HfKAHBjcXsrnvtjI0CBzoAT4.tar.br",
}

import pf.Stdout
import parser.Parser exposing [Parser]
import parser.String exposing [string, digits]
import "day5test.txt" as day5TestText : Str
import "day5.txt" as day5Text : Str

main =
    Stdout.line! "Test 1: $(part1 day5TestText)"
    Stdout.line! "Part 1: $(part1 day5Text)"
    Stdout.line! "Test 2: $(part2 day5TestText)"
    Stdout.line! "Part 2: $(part2 day5Text)"

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

Model : { rules : Dict U64 (Set U64), manuals : List (List U64) }

doPart1 : Str -> Result U64 [ParsingFailure Str, ParsingIncomplete Str, NotDoneYet]
doPart1 = \input ->
    model = String.parseStr? inputParser input
    manualIsValid = \manual ->
        checkValidity manual model.rules
    model.manuals
    |> List.keepIf manualIsValid
    |> List.keepOks findMiddlePageNumber
    |> List.sum
    |> Ok

doPart2 : Str -> Result U64 [ParsingFailure Str, ParsingIncomplete Str, NotDoneYet]
doPart2 = \input ->
    model = String.parseStr? inputParser input
    manualIsValid = \manual ->
        checkValidity manual model.rules
    putInCorrectOrder = \manual ->
        putManualInOrder manual model.rules
    model.manuals
    |> List.dropIf manualIsValid
    |> List.map putInCorrectOrder
    |> List.keepOks findMiddlePageNumber
    |> List.sum
    |> Ok

makeModel : List (U64, U64) -> (List (List U64) -> Model)
makeModel = \rulePairs -> \rawManuals ->
        rules =
            rulePairs
            |> List.walk (Dict.empty {}) addToDict
        manuals = List.dropIf rawManuals List.isEmpty
        {
            rules,
            manuals,
        }

addToDict : Dict U64 (Set U64), (U64, U64) -> Dict U64 (Set U64)
addToDict = \soFar, (from, to) ->
    Dict.update soFar from \result ->
        when result is
            Ok set -> Ok (Set.insert set to)
            Err Missing -> Ok (Set.single to)

inputParser : Parser String.Utf8 Model
inputParser =
    Parser.const makeModel
    |> Parser.keep (Parser.sepBy ruleParser (string "\r\n"))
    |> Parser.skip (string "\r\n")
    |> Parser.keep (Parser.sepBy manualParser (string "\r\n"))

ruleParser : Parser String.Utf8 (U64, U64)
ruleParser =
    Parser.const (\a -> \b -> (a, b))
    |> Parser.keep digits
    |> Parser.skip (string "|")
    |> Parser.keep digits

manualParser : Parser String.Utf8 (List U64)
manualParser =
    Parser.sepBy digits (string ",")

checkValidity : List U64, Dict U64 (Set U64) -> Bool
checkValidity = \manual, rules ->
    state = List.walkUntil manual (Bool.true, Set.empty {}) \(_, previous), page ->
        theseCannotBeBefore = Dict.get rules page |> Result.withDefault (Set.empty {})
        if Set.isEmpty (Set.intersection theseCannotBeBefore previous) then
            Continue (Bool.true, Set.insert previous page)
        else
            Break (Bool.false, previous)
    state.0

findMiddlePageNumber : List U64 -> Result U64 [ListWasEmpty]
findMiddlePageNumber = \list ->
    list
    |> List.dropFirst (List.len list // 2)
    |> List.first

putManualInOrder : List U64, Dict U64 (Set U64) -> List U64
putManualInOrder = \manual, rules ->
    orderRecursively manual [] rules

orderRecursively : List U64, List U64, Dict U64 (Set U64) -> List U64
orderRecursively = \remaining, soFar, rules ->
    if List.isEmpty remaining then
        soFar
    else
        pagesThatMustBeAfter : Set U64
        pagesThatMustBeAfter =
            List.keepOks remaining (\page -> Dict.get rules page)
            |> List.walk (Set.empty {}) Set.union
        pagesThatCanBeNow = List.dropIf remaining (\page -> Set.contains pagesThatMustBeAfter page)
        when pagesThatCanBeNow is
            [page] -> orderRecursively (List.dropIf remaining (\p -> p == page)) (List.append soFar page) rules
            [] -> crash "no pages can be now"
            _ -> crash "two or more pages can be now"
