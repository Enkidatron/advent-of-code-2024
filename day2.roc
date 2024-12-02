app [main] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br" }

import pf.Stdout
import "day2.txt" as day2Text : Str 

lines = Str.splitOn day2Text "\r\n"

reports = List.map lines toReport 
    |> List.dropIf List.isEmpty
toReport = \line -> 
    line 
        |> Str.splitOn " " 
        |> List.keepOks Str.toI64

safeReports = List.keepIf reports \report -> checkSafety report == Safe 
checkSafety = \report -> 
    changes = when report is 
        [] -> []
        [_, .. as rest] -> 
            List.map2 report rest \x,y -> x - y 
    if List.all changes (\x -> x > 0 && x <= 3) then 
        Safe 
    else if List.all changes (\x -> x < 0 && x >= -3) then 
        Safe 
    else
        # dbg report 
        Unsafe 

safeReportsWithDampener = List.keepIf reports \report -> checkSafetyWithDampener report == Safe 
checkSafetyWithDampener = \report -> 
    List.range { start : At 0, end: Before (List.len report)}
        |> List.map (\i -> 
            List.dropAt report i 
        )
        |> List.map checkSafety 
        |> findAnyThatAreSafe 
findAnyThatAreSafe = \safetyResults -> 
    if List.any safetyResults (\r -> r == Safe) then 
        Safe 
    else 
        Unsafe 


main =
    # dbg safeReports
    Stdout.line! "Part 1: $(List.len safeReports |> Num.toStr)"
    Stdout.line! "Part 2: $(List.len safeReportsWithDampener |> Num.toStr)"
