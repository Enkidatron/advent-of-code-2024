app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br",
    parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.9.0/w8YKp2YAgQt5REYk912HfKAHBjcXsrnvtjI0CBzoAT4.tar.br",
    # array2d: "https://github.com/mulias/roc-array2d/releases/download/v0.3.1/2Jqajvxn36vRryyQBSluU6Fo6vVI5yNSYmcJcyaKp0Y.tar.br",
}

import pf.Stdout
import pf.Utc
import parser.Parser exposing [Parser, skip, keep, chompUntil, chompWhile]
import parser.String exposing [string, codeunit]
# import array2d.Array2D
import "day23test.txt" as testText : Str
import "day23.txt" as puzzleText : Str

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
        Ok answer -> answer
        Err _ -> "Error"

doPart1 : Str -> Result U64 _
doPart1 = \input ->
    connections = String.parseStr? connectionsParser input
    findAllTrios connections
    |> Set.keepIf connectionContainsTName
    |> Set.len
    |> Ok

doPart2 : Str -> Result Str _
doPart2 = \input ->
    connections = String.parseStr? connectionsParser input
    findLargestLanParty connections
    |> Set.toList
    |> List.sortWith compStr
    |> Str.joinWith ","
    |> Ok

connectionsParser : Parser _ (Dict Str (Set Str))
connectionsParser =
    Parser.const (\a -> \b -> (extractFromUtf8 a, extractFromUtf8 b))
    |> keep (chompUntil '-')
    |> skip (codeunit '-')
    |> keep (chompWhile (\char -> char != '\r'))
    |> Parser.sepBy (string "\r\n")
    |> Parser.map
        (\connections ->
            List.walk connections (Dict.empty {}) \soFar, (left, right) ->
                soFar
                |> addValueToDictSet left right
                |> addValueToDictSet right left
        )

extractFromUtf8 = \list ->
    when Str.fromUtf8 list is
        Ok str -> str
        Err _ -> crash "bad utf8 string"

addValueToDictSet : Dict a (Set b), a, b -> Dict a (Set b)
addValueToDictSet = \dict, k, v ->
    Dict.update dict k \maybeVal ->
        when maybeVal is
            Ok set -> Ok (Set.insert set v)
            Err _ -> Ok (Set.single v)

findAllTrios : Dict Str (Set Str) -> Set (Str, Str, Str)
findAllTrios = \dict ->
    findTriosForKey = \triosSoFar, first, connectedTo ->
        Set.walk connectedTo triosSoFar \innerSoFar, second ->
            when Dict.get dict second is
                Ok connectedToSecond ->
                    connectedToBoth = Set.intersection connectedTo connectedToSecond
                    newConnections = Set.map connectedToBoth (\third -> makeConnection first second third)
                    Set.union innerSoFar newConnections

                Err _ -> crash "couldn't find second, this shouldn't happen"
    Dict.walk dict (Set.empty {}) findTriosForKey

makeConnection = \a, b, c ->
    when [a, b, c] |> List.sortWith compStr is
        [x, y, z] -> (x, y, z)
        _ -> crash "bad makeConnection"

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

connectionContainsTName = \(a, b, c) ->
    Str.startsWith a "t" || Str.startsWith b "t" || Str.startsWith c "t"

findLargestLanParty : Dict Str (Set Str) -> Set Str
findLargestLanParty = \dict ->
    help : Set Str, Set Str, Set Str -> Set Str
    help = \biggest, party, canJoin ->
        # dbg party
        # dbg (Set.len party)
        # dbg (Set.len canJoin)
        # dbg (Set.len biggest)
        if Set.isEmpty canJoin then
            if Set.len party > Set.len biggest then
                dbg party
            else
                biggest
        else if Set.len party + Set.len canJoin <= Set.len biggest then
            biggest
        else if ((Set.len party) + (Set.len canJoin) == (Set.len biggest) + 1) then
            possibleParty = Set.union party canJoin
            if isClique dict possibleParty then
                dbg possibleParty
            else
                biggest
        else
            Set.walk canJoin biggest \innerBiggest, newMember ->
                newConnections = getOrCrash dict newMember
                newParty = Set.insert party newMember
                newCanJoin = Set.intersection canJoin newConnections
                help innerBiggest newParty newCanJoin
    findLanPartiesForKey : Set Str, Str, Set Str -> Set Str
    findLanPartiesForKey = \biggestSoFar, first, connectedTo ->
        # dbg first
        # dbg (Set.len connectedTo)
        help biggestSoFar (Set.single first) connectedTo

    Dict.walk dict (Set.empty {}) findLanPartiesForKey

getOrCrash : Dict k v, k -> v
getOrCrash = \dict, k ->
    when Dict.get dict k is
        Ok v -> v
        Err _ -> crash "bad lookup"

isClique : Dict Str (Set Str), Set Str -> Bool
isClique = \dict, party ->
    Set.walkUntil party Bool.true \_, member ->
        memberConnections = getOrCrash dict member
        disconnected = Set.difference party memberConnections |> Set.remove member
        if Set.isEmpty disconnected then
            Continue Bool.true
        else
            Break Bool.false
