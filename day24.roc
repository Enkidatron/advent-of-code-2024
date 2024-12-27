app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br",
    parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.9.0/w8YKp2YAgQt5REYk912HfKAHBjcXsrnvtjI0CBzoAT4.tar.br",
    # array2d: "https://github.com/mulias/roc-array2d/releases/download/v0.3.1/2Jqajvxn36vRryyQBSluU6Fo6vVI5yNSYmcJcyaKp0Y.tar.br",
}

import pf.Stdout
import pf.Utc
import parser.Parser exposing [Parser, skip, keep, chompWhile]
import parser.String exposing [string, digit]
# import array2d.Array2D
import "day24test.txt" as testText : Str
import "day24.txt" as puzzleText : Str

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
    when doPart1 input |> dbg is
        Ok answer -> Num.toStr answer
        Err _ -> "Error"

part2 : Str -> Str
part2 = \input ->
    when doPart2 input is
        Ok answer -> answer
        Err _ -> "Error"

doPart1 : Str -> Result U64 _
doPart1 = \input ->
    device = String.parseStr? deviceParser input
    newDevice = operate device |> orCrash
    readZOutput newDevice

doPart2 : Str -> Result Str _
doPart2 = \input ->
    device = String.parseStr? deviceParser input
    swaps = [] # answer omitted, manually found through crashes and examination
    deviceToCheck = { device & gates: swapGates device.gates swaps }
    # dbg (findSwapsThatHelp device)
    originalGatesByOutput = deviceToCheck.gates |> List.map (\gate -> (gate.out, [gate.in1, gate.in2])) |> Dict.fromList
    newWiresHelp = \usedWires, wireName, soFar ->
        candidates =
            Dict.get originalGatesByOutput wireName
            |> Result.withDefault []
            |> List.dropIf (\w -> Set.contains usedWires w || Set.contains soFar w)
        # |> List.dropIf (\w -> Str.startsWith w "x" || Str.startsWith w "y")
        List.walk candidates (soFar) \innerSoFar, candidateName ->
            newWiresHelp usedWires candidateName (Set.insert innerSoFar candidateName)
    (_usedByAllCurrentZWires, newWiresByZ) = List.walk deviceToCheck.zWires (Set.empty {}, Dict.empty {}) \(allWiresUsed, soFar), zWire ->
        newWiresForThisZ = newWiresHelp allWiresUsed zWire (Set.empty {})
        (Set.union allWiresUsed newWiresForThisZ, Dict.insert soFar zWire (Set.insert newWiresForThisZ zWire))
    newDepsByZ = newWiresByZ |> Dict.map (\_zName, deps -> (deps |> Set.keepIf (\w -> Str.startsWith w "x" || Str.startsWith w "y"), deps |> Set.dropIf (\w -> Str.startsWith w "x" || Str.startsWith w "y")))
    potentialProblems = newDepsByZ |> Dict.dropIf (\(zName, (inputs, intermediates)) -> (Set.len inputs == 2 && Set.len intermediates == 5) || zName == "z00" || zName == "z01")
    dbg potentialProblems

    _ =
        device.gates
        |> List.map
            (\gate ->
                if gate.out |> Str.startsWith "z" && gate.op != Xor then
                    _problem = gate |> dbg
                    {}
                else
                    {}
            )
    gateDefs =
        List.range { start: At 1, end: At 44 }
            |> List.walk
                (Dict.fromList [("spq", Carry 0)])
                (\soFar, n ->
                    # dbg soFar
                    # identify a sub n
                    maybeAGate =
                        deviceToCheck.gates
                            |> List.keepOks
                                (\gate ->
                                    inNum1 = Str.toU64? (gate.in1 |> Str.dropPrefix "x" |> Str.dropPrefix "y")
                                    inNum2 = Str.toU64? (gate.in2 |> Str.dropPrefix "x" |> Str.dropPrefix "y")
                                    if inNum1 == n && inNum2 == n && gate.op == Xor then Ok gate else Err NotThisGate
                                )
                            |> List.first
                    addAGate = \dict ->
                        when maybeAGate is
                            Ok gate ->
                                Dict.insert dict gate.out (A n)
                                |> \thing ->
                                    if gate.out |> Str.startsWith "z" then
                                        _ = dbg gate
                                        thing
                                    else
                                        thing

                            Err _ -> crash "could not find a-sub-n for n=$(Num.toStr n)"

                    maybeBGate =
                        deviceToCheck.gates
                            |> List.keepOks
                                (\gate ->
                                    inNum1 = Str.toU64? (gate.in1 |> Str.dropPrefix "x" |> Str.dropPrefix "y")
                                    inNum2 = Str.toU64? (gate.in2 |> Str.dropPrefix "x" |> Str.dropPrefix "y")
                                    if inNum1 == n && inNum2 == n && gate.op == And then Ok gate else Err NotThisGate
                                )
                            |> List.first
                    addBGate = \dict ->
                        when maybeBGate is
                            Ok gate ->
                                Dict.insert dict gate.out (B n)
                                |> \thing ->
                                    if gate.out |> Str.startsWith "z" then
                                        _ = dbg gate
                                        thing
                                    else
                                        thing

                            Err _ -> crash "could not find b-sub-n for n=$(Num.toStr n)"
                    intermediateDict = soFar |> addAGate |> addBGate |> dbg
                    maybeDGate =
                        deviceToCheck.gates
                            |> List.keepOks
                                (\gate ->
                                    inRole1 = Dict.get? intermediateDict gate.in1
                                    inRole2 = Dict.get? intermediateDict gate.in2
                                    if matchesIgnoringOrder (inRole1, inRole2) (A n, Carry (n - 1)) && gate.op == And then Ok gate else Err NotThisGate
                                )
                            |> List.first
                    addDGate = \dict ->
                        when maybeDGate is
                            Ok gate ->
                                Dict.insert dict gate.out (D n)
                                |> \thing ->
                                    if gate.out |> Str.startsWith "z" then
                                        _ = dbg gate
                                        thing
                                    else
                                        thing

                            Err _ ->
                                _ = dbg "could not find d-sub-n for n=$(Num.toStr n)"
                                dbg dict

                                crash "stop at bad d gate"
                    intermediate2Dict = intermediateDict |> addDGate
                    maybeCGate =
                        deviceToCheck.gates
                            |> List.keepOks
                                (\gate ->
                                    inRole1 = Dict.get? intermediate2Dict gate.in1
                                    inRole2 = Dict.get? intermediate2Dict gate.in2
                                    if matchesIgnoringOrder (inRole1, inRole2) (D n, B n) && gate.op == Or then Ok gate else Err NotThisGate
                                )
                            |> List.first
                    addCGate = \dict ->
                        when maybeCGate is
                            Ok gate ->
                                Dict.insert dict gate.out (Carry n)
                                |> \thing ->
                                    if gate.out |> Str.startsWith "z" then
                                        _ = dbg gate
                                        thing
                                    else
                                        thing

                            Err _ ->
                                _ = dbg "could not find c-sub-n for n=$(Num.toStr n)"
                                # crash "could not find c-sub-n for n=$(Num.toStr n)"
                                dbg dict

                                crash "stop at bad c gate"
                    intermediate2Dict |> addCGate |> dbg
                )
    dbg gateDefs

    swaps
    |> List.joinMap (\(left, right) -> [left, right])
    |> List.sortWith compStr
    |> Str.joinWith ","
    |> Ok

Device : { activeWires : Dict Str Bool, gates : List Gate, zWires : List Str, xWires : List Str, yWires : List Str }
Gate : { in1 : Str, op : Op, in2 : Str, out : Str }
Op : [And, Or, Xor]

deviceParser : Parser _ Device
deviceParser =
    Parser.const
        (\activeWires -> \gates -> {
                activeWires,
                gates,
                zWires: gates |> List.map .out |> List.keepIf (\s -> Str.startsWith s "z") |> List.sortWith (\a, b -> Num.compare (nameToNum a) (nameToNum b)),
                xWires: activeWires |> Dict.keys |> List.keepIf (\s -> Str.startsWith s "x") |> List.sortWith (\a, b -> Num.compare (nameToNum a) (nameToNum b)),
                yWires: activeWires |> Dict.keys |> List.keepIf (\s -> Str.startsWith s "y") |> List.sortWith (\a, b -> Num.compare (nameToNum a) (nameToNum b)),
            })
    |> keep (activeWiresParser |> Parser.map Dict.fromList)
    |> skip (string "\r\n\r\n")
    |> keep gatesParser
activeWiresParser =
    Parser.const (\a -> \b -> (a, b))
    |> keep idParser
    |> skip (string ": ")
    |> keep (digit |> Parser.map (\x -> x == 1))
    |> Parser.sepBy (string "\r\n")

idParser =
    chompWhile (\char -> char != ':' && char != ' ' && char != '\r') |> Parser.map fromUtf8OrCrash
fromUtf8OrCrash = \utf8 ->
    when Str.fromUtf8 utf8 is
        Ok str -> str
        Err _ -> crash "bad string during parsing"
gatesParser =
    Parser.const (\in1 -> \op -> \in2 -> \out -> { in1, op, in2, out })
    |> keep idParser
    |> skip (string " ")
    |> keep opParser
    |> skip (string " ")
    |> keep idParser
    |> skip (string " -> ")
    |> keep idParser
    |> Parser.sepBy (string "\r\n")
opParser =
    Parser.oneOf [
        Parser.const And |> skip (string "AND"),
        Parser.const Or |> skip (string "OR"),
        Parser.const Xor |> skip (string "XOR"),
    ]

operate : Device -> Result Device [DeviceIsStuck Device]
operate = \device ->
    # dbg device.zWires
    when device.gates is
        [] -> Ok device
        gates ->
            if List.all device.zWires (\zWire -> Dict.contains device.activeWires zWire) then
                Ok device
            else
                newDevice = List.walk gates { device & gates: [] } stepDevice
                if List.len newDevice.gates == List.len gates then
                    Err (DeviceIsStuck newDevice)
                else
                    operate newDevice
stepDevice : Device, Gate -> Device
stepDevice = \device, gate ->
    input1 = Dict.get device.activeWires gate.in1
    input2 = Dict.get device.activeWires gate.in2
    when (input1, input2) is
        (Ok in1, Ok in2) ->
            { device & activeWires: Dict.insert device.activeWires gate.out (eval in1 gate.op in2) }

        _ ->
            { device & gates: List.append device.gates gate }
eval : Bool, Op, Bool -> Bool
eval = \in1, op, in2 ->
    when op is
        And -> in1 && in2
        Or -> in1 || in2
        Xor -> (in1 || in2) && (Bool.not (in1 && in2))

readZOutput : Device -> Result U64 _
readZOutput = \device ->
    readOutput device device.zWires

readOutput : Device, List Str -> Result U64 _
readOutput = \device, wireNames ->
    readWire : Str -> Result (U64, Bool) _
    readWire = \name ->
        val = Dict.get? device.activeWires name
        num = Str.toU64? (name |> Str.dropPrefix "z" |> Str.dropPrefix "x" |> Str.dropPrefix "y")
        Ok (num, val)
    allWires : List (U64, Bool)
    allWires =
        wireNames
            |> List.mapTry? readWire
    allWires
    |> List.sortWith (\a, b -> Num.compare a.0 b.0)
    |> List.map .1
    |> List.walkWithIndex 0 \soFar, bool, index ->
        if bool then
            soFar + (Num.powInt 2 index)
        else
            soFar
    |> Ok
nameToNum : Str -> U64
nameToNum = \name ->
    name |> Str.dropPrefix "z" |> Str.dropPrefix "x" |> Str.dropPrefix "y" |> Str.toU64 |> orCrashWith "bad name to num: $(name)"

orCrash : Result a * -> a
orCrash = \result ->
    when result is
        Ok a -> a
        Err _ -> crash "welp, that happened"
orCrashWith : Result a *, Str -> a
orCrashWith = \result, text ->
    when result is
        Ok a -> a
        Err _ -> crash text

matchesIgnoringOrder : (a, a), (a, a) -> Bool where a implements Eq
matchesIgnoringOrder = \(a, b), (x, y) ->
    (a == x && b == y) || (a == y && b == x)

compStr = \a, b ->
    help = \listA, listB ->
        when (listA, listB) is
            ([firstA, .. as restA], [firstB, .. as restB]) ->
                comp = Num.compare firstA firstB
                when comp is
                    EQ -> help restA restB
                    _ -> comp

            ([], []) -> EQ
            _ -> crash "unequal strings"
    help (Str.toUtf8 a) (Str.toUtf8 b)
Swap : (Str, Str)
swapGates : List Gate, List Swap -> List Gate
swapGates = \gates, swaps ->
    nameChanges = List.walk swaps (Dict.empty {}) \soFar, (first, second) ->
        soFar |> Dict.insert first second |> Dict.insert second first
    List.map gates \gate ->
        when Dict.get nameChanges gate.out is
            Ok newOut -> { gate & out: newOut }
            Err _ -> gate
