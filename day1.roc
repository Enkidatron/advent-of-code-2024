app [main] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br" }

import pf.Stdout
import "day1.txt" as day1Text : Str

listOfBoth = Str.splitOn day1Text "\r\n"
bothLists = List.walk
    listOfBoth
    ([], [])
    (\(leftSoFar, rightSoFar), next ->
        when Str.splitOn next "   " is
            [newLeft, newRight] ->
                (List.prependIfOk leftSoFar (Str.toI64 newLeft), List.prependIfOk rightSoFar (Str.toI64 newRight))

            _ -> crash "This shouldn't happen"
    )
leftList = bothLists.0
rightList = bothLists.1
sortedLeft = List.sortAsc leftList
sortedRight = List.sortAsc rightList
distances = List.map2
    sortedLeft
    sortedRight
    (\left, right ->
        Num.abs (left - right)
    )
totalDistance = List.sum distances

# Part 2
rightCounts = List.walk rightList (Dict.empty {}) addToCountsDict
similarityScores = List.map
    leftList
    (\nextLeft ->
        count = Result.withDefault (Dict.get rightCounts nextLeft) 0
        nextLeft * count
    )

totalSimilarity = List.sum similarityScores

main =
    # Stdout.line! "$(List.len distances |> Num.toStr) $(List.len sortedRight |> Num.toStr)"
    Stdout.line! "Total Distance: $(Num.toStr totalDistance)"
    Stdout.line! "Total Similarity: $(Num.toStr totalSimilarity)"

addToCountsDict = \dictSoFar, key ->
    Dict.update
        dictSoFar
        key
        (\maybeVal ->
            when maybeVal is
                Err Missing -> Ok 1
                Ok count -> Ok (count + 1)
        )
