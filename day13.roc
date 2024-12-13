app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br",
    parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.9.0/w8YKp2YAgQt5REYk912HfKAHBjcXsrnvtjI0CBzoAT4.tar.br",
    array2d: "https://github.com/mulias/roc-array2d/releases/download/v0.3.1/2Jqajvxn36vRryyQBSluU6Fo6vVI5yNSYmcJcyaKp0Y.tar.br",
}

import pf.Stdout
import pf.Utc
import parser.Parser exposing [Parser, keep, skip]
import parser.String exposing [digits, string]
import array2d.Array2D
import "day13test.txt" as testText : Str
import "day13.txt" as puzzleText : Str

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
    machines = String.parseStr? parseMachines input
    costs = List.keepOks machines findCost
    Ok (List.sum costs)

doPart2 : Str -> Result U64 _
doPart2 = \input ->
    machines = String.parseStr? parseMachines input
    adjustedMachines = List.map machines \machine -> { machine & prize: { x: machine.prize.x + 10000000000000, y: machine.prize.y + 10000000000000 } }
    costs = List.keepOks adjustedMachines findBigCost
    Ok (List.sum costs)

Point : { x : U64, y : U64 }
Machine : { buttonA : { x : U64, y : U64 }, buttonB : { x : U64, y : U64 }, prize : Point }

parseMachines : Parser _ (List Machine)
parseMachines =
    Parser.sepBy machineParser (string "\r\n\r\n")

machineParser =
    Parser.const (\buttonA -> \buttonB -> \prize -> { buttonA, buttonB, prize })
    |> keep buttonParser
    |> skip (string "\r\n")
    |> keep buttonParser
    |> skip (string "\r\n")
    |> keep prizeParser
buttonParser =
    Parser.const (\x -> \y -> { x, y })
    |> skip (string "Button ")
    |> skip (Parser.oneOf [string "A", string "B"])
    |> skip (string ": X+")
    |> keep digits
    |> skip (string ", Y+")
    |> keep digits
prizeParser =
    Parser.const (\x -> \y -> { x, y })
    |> skip (string "Prize: X=")
    |> keep (digits)
    |> skip (string ", Y=")
    |> keep digits

findCost = \machine ->
    array = Array2D.init { rows: 100, cols: 100 } \index ->
        aPresses = index.row + 1
        bPresses = index.col + 1
        reachedPosition = {
            x: (aPresses * machine.buttonA.x) + (bPresses * machine.buttonB.x),
            y: (aPresses * machine.buttonA.y) + (bPresses * machine.buttonB.y),
        }
        { aPresses, bPresses, reachedPosition }
    cost = \{ aPresses, bPresses } ->
        (aPresses * 3) + bPresses
    array
    |> Array2D.toList
    |> List.keepIf (\combo -> combo.reachedPosition == machine.prize)
    |> List.map cost
    |> List.sortAsc
    |> List.first

findBigCost : Machine -> Result U64 _
findBigCost = \machine ->
    denom = (Num.toI64 (machine.buttonA.x * machine.buttonB.y) - Num.toI64 (machine.buttonA.y * machine.buttonB.x))
    aRounded = (Num.toI64 (machine.prize.x * machine.buttonB.y) - Num.toI64 (machine.prize.y * machine.buttonB.x)) // denom |> Num.toU64Checked?
    bTop = (Num.toI64 (machine.prize.y * machine.buttonA.x) - Num.toI64 (machine.prize.x * machine.buttonA.y))
    bRounded = bTop // denom |> Num.toU64Checked?
    combo = (
        if aRounded * machine.buttonA.x + bRounded * machine.buttonB.x == machine.prize.x && aRounded * machine.buttonA.y + bRounded * machine.buttonB.y == machine.prize.y then
            Ok { aPresses: aRounded, bPresses: bRounded }
            else

        Err NoFactorFound
    )
    cost = \{ aPresses, bPresses } ->
        (aPresses * 3) + bPresses
    Result.map (combo) cost
