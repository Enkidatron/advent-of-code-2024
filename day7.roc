app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br",
    parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.9.0/w8YKp2YAgQt5REYk912HfKAHBjcXsrnvtjI0CBzoAT4.tar.br",
}

import pf.Stdout
import parser.Parser
import parser.String exposing [string, digits]
import "day7test.txt" as testText : Str
import "day7.txt" as puzzleText : Str

main =
    Stdout.line! "Test 1: $(part1 testText)"
    Stdout.line! "Part 1: $(part1 puzzleText)"
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

Equation : { target : U64, operands : List U64 }
Operator : [Add, Multiply, Concat]

doPart1 : Str -> Result U64 _
doPart1 = \input ->
    equations = String.parseStr? equationsParser input
    equations
    |> List.keepIf equationCanWork
    |> List.map .target
    |> List.sum
    |> Ok
doPart2 : Str -> Result U64 _
doPart2 = \input ->
    equations = String.parseStr? equationsParser input
    equations
    |> List.keepIf equationCanWork2
    |> List.map .target
    |> List.sum
    |> Ok

equationsParser =
    Parser.sepBy equationParser (string "\r\n")

equationParser =
    Parser.const (\target -> \operands -> { target, operands })
    |> Parser.keep (digits)
    |> Parser.skip (string ": ")
    |> Parser.keep (Parser.sepBy (digits) (string " "))

equationCanWork : Equation -> Bool
equationCanWork = \equation ->
    reachableTargets = List.walk equation.operands (Set.empty {}) \setSoFar, operand ->
        if Set.isEmpty setSoFar then
            Set.single operand
        else
            Set.joinMap setSoFar \numSoFar ->
                [Add, Multiply]
                |> List.map (\op -> applyOp op numSoFar operand)
                |> Set.fromList
    Set.contains reachableTargets equation.target

equationCanWork2 : Equation -> Bool
equationCanWork2 = \equation ->
    reachableTargets = List.walk equation.operands (Set.empty {}) \setSoFar, operand ->
        if Set.isEmpty setSoFar then
            Set.single operand
        else
            Set.joinMap setSoFar \numSoFar ->
                [Add, Multiply, Concat]
                |> List.map (\op -> applyOp op numSoFar operand)
                |> Set.fromList
                |> Set.keepIf (\newNum -> newNum <= equation.target)
    Set.contains reachableTargets equation.target

applyOp : Operator, U64, U64 -> U64
applyOp = \operator, x, y ->
    when operator is
        Add -> x + y
        Multiply -> x * y
        Concat ->
            Str.concat (Num.toStr x) (Num.toStr y)
            |> Str.toU64
            |> \r ->
                when r is
                    Ok num -> num
                    Err InvalidNumStr -> crash "bad num"
