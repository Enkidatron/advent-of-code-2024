app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br",
    array2d: "https://github.com/mulias/roc-array2d/releases/download/v0.3.1/2Jqajvxn36vRryyQBSluU6Fo6vVI5yNSYmcJcyaKp0Y.tar.br",
}

import pf.Stdout
import pf.Utc
import array2d.Array2D exposing [Array2D]
import array2d.Index2D exposing [Index2D]
import array2d.Shape2D exposing [Shape2D]
import "day12test.txt" as testText : Str
import "day12.txt" as puzzleText : Str

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
    map =
        input
            |> Str.splitOn "\r\n"
            |> List.map (\line -> line |> Str.toUtf8 |> List.keepOks (\char -> Str.fromUtf8 [char]))
            |> Array2D.fromExactLists?
    regions = findRegions map
    mapShape = Array2D.shape map
    cost =
        List.map regions (\region -> costOfRegion region mapShape)
        |> List.sum
    Ok cost

doPart2 : Str -> Result U64 _
doPart2 = \input ->
    map =
        input
            |> Str.splitOn "\r\n"
            |> List.map (\line -> line |> Str.toUtf8 |> List.keepOks (\char -> Str.fromUtf8 [char]))
            |> Array2D.fromExactLists?
    regions = findRegions map
    mapShape = Array2D.shape map
    cost =
        List.map regions (\region -> costOfRegionWithBulkDiscount region mapShape)
        |> List.sum
    Ok cost

Region : { label : Str, plots : Set Index2D }

findRegions : Array2D Str -> List Region
findRegions = \map ->
    outcome = Array2D.walk
        map
        ([], Set.empty {})
        { direction: Forwards }
        (\(regionsSoFar, foundPlotsSoFar), plantLabel, thisPlot ->
            if Set.contains foundPlotsSoFar thisPlot then
                (regionsSoFar, foundPlotsSoFar) # This plot is already in a mapped region, move along
            else
                plotsInThisRegion = findContiguousPlotsFromHere map plantLabel thisPlot
                thisRegion = { label: plantLabel, plots: plotsInThisRegion }
                (List.append regionsSoFar thisRegion, Set.union foundPlotsSoFar plotsInThisRegion)
        )
    outcome.0

findContiguousPlotsFromHere = \map, ourLabel, startingPlot ->
    mapShape = Array2D.shape map
    help = \regionSoFar, plot ->
        [(NextRow, SameCol), (PrevRow, SameCol), (SameRow, NextCol), (SameRow, PrevCol)]
        |> List.keepOks (\(a, b) -> Index2D.adjacentTo plot mapShape a b)
        |> List.dropIf (\nextPlot -> Set.contains regionSoFar nextPlot)
        |> List.keepIf (\nextPlot -> Array2D.get map nextPlot == Ok ourLabel)
        |> List.walk regionSoFar (\innerSoFar, nextPlot -> help (Set.insert innerSoFar nextPlot) nextPlot)
    help (Set.single startingPlot) startingPlot

costOfRegion : Region, Shape2D -> U64
costOfRegion = \region, mapShape ->
    area = Set.len region.plots
    openSides = \plot ->
        neighborsInPlot =
            [(NextRow, SameCol), (PrevRow, SameCol), (SameRow, NextCol), (SameRow, PrevCol)]
            |> List.keepOks (\(a, b) -> Index2D.adjacentTo plot mapShape a b)
            |> List.keepIf (\otherPlot -> Set.contains region.plots otherPlot)
        4 - (List.len neighborsInPlot)
    perimeter =
        Set.toList region.plots
        |> List.map openSides
        |> List.sum
    area * perimeter

costOfRegionWithBulkDiscount : Region, Shape2D -> U64
costOfRegionWithBulkDiscount = \region, mapShape ->
    area = Set.len region.plots
    fenceSegmentStartsSide = \dir, plot ->
        (towardsFence, towardsNeighbor) =
            when dir is
                Left -> ((SameRow, PrevCol), (PrevRow, SameCol))
                Up -> ((PrevRow, SameCol), (SameRow, PrevCol))
                Right -> ((SameRow, NextCol), (PrevRow, SameCol))
                Down -> ((NextRow, SameCol), (SameRow, PrevCol))
        thereIsAFence =
            when Index2D.adjacentTo plot mapShape towardsFence.0 towardsFence.1 is
                Err OutOfBounds -> Bool.true
                Ok acrossTheFence -> Set.contains region.plots acrossTheFence |> Bool.not
        neighborHasContiguousFence =
            when Index2D.adjacentTo plot mapShape towardsNeighbor.0 towardsNeighbor.1 is
                Err OutOfBounds -> Bool.false
                Ok neighbor ->
                    if Set.contains region.plots neighbor then
                        when Index2D.adjacentTo neighbor mapShape towardsFence.0 towardsFence.1 is
                            Err OutOfBounds -> Bool.true
                            Ok diagonalPlot -> Set.contains region.plots diagonalPlot |> Bool.not
                    else
                        Bool.false
        thereIsAFence && (Bool.not neighborHasContiguousFence)
    openSidesThatStartFences = \plot ->
        [Left, Up, Right, Down]
        |> List.keepIf (\dir -> fenceSegmentStartsSide dir plot)
        |> List.len
    numSides =
        Set.toList region.plots
        |> List.map openSidesThatStartFences
        |> List.sum
    area * numSides
