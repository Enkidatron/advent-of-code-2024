app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br",
    parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.9.0/w8YKp2YAgQt5REYk912HfKAHBjcXsrnvtjI0CBzoAT4.tar.br",
    # array2d: "https://github.com/mulias/roc-array2d/releases/download/v0.3.1/2Jqajvxn36vRryyQBSluU6Fo6vVI5yNSYmcJcyaKp0Y.tar.br",
}

import pf.Stdout
import pf.Utc
import parser.Parser exposing [Parser, keep, skip]
import parser.String exposing [string, digits, digit]
# import array2d.Array2D
import "day17test.txt" as testText : Str
import "day17.txt" as puzzleText : Str

time = \{} -> Utc.now {} |> Task.map Utc.toMillisSinceEpoch
main =
    Stdout.write! puzzleText
    Stdout.line! ""
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
        Ok answer -> answer
        Err _ -> "Error"

part2 : Str -> Str
part2 = \input ->
    when doPart2 input is
        Ok num -> Num.toStr num
        Err _ -> "Error"

doPart1 : Str -> Result Str [NoValueFound, ParsingFailure Str, ParsingIncomplete Str]
doPart1 = \input ->
    vm = String.parseStr? vmParser input
    runUntilHalt vm
    |> .output
    |> List.map Num.toStr
    |> Str.joinWith ","
    |> Ok

doPart2 : Str -> Result U64 _
doPart2 = \input ->
    vm = String.parseStr? vmParser input
    findSelfGeneratingValue vm

VM : { a : U64, b : U64, c : U64, opPointer : U64, output : List U8, program : List U8 }

vmParser : Parser _ VM
vmParser =
    Parser.const (\a -> \b -> \c -> \program -> { a, b, c, program, opPointer: 0, output: [] })
    |> skip (string "Register A: ")
    |> keep (digits)
    |> skip (string "\r\nRegister B: ")
    |> keep digits
    |> skip (string "\r\nRegister C: ")
    |> keep digits
    |> skip (string "\r\n\r\nProgram: ")
    |> keep (Parser.sepBy (Parser.map digit Num.toU8) (string ","))

runUntilHalt : VM -> VM
runUntilHalt = \vm ->
    when stepVM vm is
        Halt -> vm
        Step newVM -> runUntilHalt newVM
stepVM : VM -> [Halt, Step VM]
stepVM = \vm ->
    when (List.get vm.program vm.opPointer, List.get vm.program (vm.opPointer + 1)) is
        (Ok 0, Ok operand) ->
            newA = vm.a // (Num.powInt 2 (getComboOp operand vm))
            Step { vm & a: newA, opPointer: vm.opPointer + 2 }

        (Ok 1, Ok operand) ->
            newB = Num.bitwiseXor vm.b (Num.toU64 operand)
            Step { vm & b: newB, opPointer: vm.opPointer + 2 }

        (Ok 2, Ok operand) ->
            newB = (getComboOp operand vm) % 8
            Step { vm & b: newB, opPointer: vm.opPointer + 2 }

        (Ok 3, Ok operand) ->
            newOpPointer =
                if vm.a == 0 then
                    vm.opPointer + 2
                else
                    Num.toU64 operand
            Step { vm & opPointer: newOpPointer }

        (Ok 4, Ok _operand) ->
            newB = Num.bitwiseXor vm.b vm.c
            Step { vm & b: newB, opPointer: vm.opPointer + 2 }

        (Ok 5, Ok operand) ->
            newOutput = getComboOp operand vm |> Num.rem 8 |> Num.toU8
            Step { vm & output: List.append vm.output newOutput, opPointer: vm.opPointer + 2 }

        (Ok 6, Ok operand) ->
            newB = vm.a // (Num.powInt 2 (getComboOp operand vm))
            Step { vm & b: newB, opPointer: vm.opPointer + 2 }

        (Ok 7, Ok operand) ->
            newC = vm.a // (Num.powInt 2 (getComboOp operand vm))
            Step { vm & c: newC, opPointer: vm.opPointer + 2 }

        _ -> Halt
getComboOp = \operand, vm ->
    when operand is
        0 | 1 | 2 | 3 -> operand |> Num.toU64
        4 -> vm.a
        5 -> vm.b
        6 -> vm.c
        _ -> crash "bad operand"

findSelfGeneratingValue : VM -> Result U64 _
findSelfGeneratingValue = \vm ->
    programLength = List.len vm.program
    help : List U64, U64, U64 -> Result U64 _
    help = \digitList, digitsToConfirm, valueToTest ->
        candidateList = List.set digitList (programLength - digitsToConfirm) valueToTest
        newA = octalListToNumber candidateList
        evaluated = runUntilHalt { vm & a: newA }
        if confirmEndsMatch vm.program evaluated.output digitsToConfirm then
            if digitsToConfirm == programLength then
                Ok newA
            else
                when help candidateList (digitsToConfirm + 1) 0 is
                    Err _ ->
                        if valueToTest >= 9 then
                            Err NoValueFound
                        else
                            help digitList digitsToConfirm (valueToTest + 1)

                    Ok answer -> Ok answer
        else if valueToTest >= 9 then
            Err NoValueFound
            else

        help digitList digitsToConfirm (valueToTest + 1)

    help (List.repeat 0 (programLength)) 1 0

octalListToNumber : List U64 -> U64
octalListToNumber = \numbers ->
    List.mapWithIndex
        numbers
        (\x, index ->
            x * (8 |> Num.powInt (index)))
    |> List.sum

confirmEndsMatch = \listA, listB, itemsToCheck ->
    if
        List.len listA == List.len listB
    then
        (List.takeLast listA itemsToCheck) == (List.takeLast listB itemsToCheck)
        else

    Bool.false

