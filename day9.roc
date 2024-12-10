app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br",
    parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.9.0/w8YKp2YAgQt5REYk912HfKAHBjcXsrnvtjI0CBzoAT4.tar.br",
}

import pf.Stdout
import pf.Utc
# import parser.Parser exposing [Parser]
# import parser.String exposing [string]
import "day9test.txt" as testText : Str
import "day9.txt" as puzzleText : Str

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
    disk = readDisk input
    # dbg disk
    newDisk = compactDisk disk
    # dbg newDisk
    Ok (checksum newDisk)

doPart2 : Str -> Result U64 _
doPart2 = \input ->
    readDisk2 input
    |> compactDisk2 # |> dbg
    |> normalize2 # |> dbg
    |> checksum
    |> Ok

readDisk = \input ->
    walked = Str.walkUtf8 input { fileId: 0, nextBlock: IsAFile, output: [] } \state, char ->
        when (Str.fromUtf8 [char] |> Result.try Str.toU64, state.nextBlock) is
            (Ok num, IsAFile) -> { state & fileId: state.fileId + 1, nextBlock: IsEmpty, output: List.concat state.output (List.repeat (File state.fileId) num) }
            (Ok num, IsEmpty) -> { state & nextBlock: IsAFile, output: List.concat state.output (List.repeat Empty num) }
            _ -> crash "whoops"
    walked.output

compactDisk = \disk ->
    compactDiskHelp disk 0 (List.len disk - 1)

compactDiskHelp = \disk, emptySpacePointer, filePointer ->
    if emptySpacePointer >= filePointer then
        disk
    else
        when List.get disk filePointer is
            Ok (File _) ->
                when List.get disk emptySpacePointer is
                    Ok Empty -> compactDiskHelp (List.swap disk emptySpacePointer filePointer) (emptySpacePointer + 1) (filePointer - 1)
                    Ok (File _) -> compactDiskHelp disk (emptySpacePointer + 1) filePointer
                    Err _ -> crash "bad empty space pointer"

            Ok Empty -> compactDiskHelp disk emptySpacePointer (filePointer - 1)
            Err _ -> crash "bad file pointer"

checksum = \disk ->
    List.mapWithIndex
        disk
        (\block, index ->
            when block is
                Empty -> 0
                File fileId -> fileId * index
        )
    |> List.sum

readDisk2 = \input ->
    walked = Str.walkUtf8 input { fileId: 0, nextBlock: IsAFile, output: [] } \state, char ->
        when (Str.fromUtf8 [char] |> Result.try Str.toU64, state.nextBlock) is
            (Ok num, IsAFile) -> { state & fileId: state.fileId + 1, nextBlock: IsEmpty, output: List.append state.output (File2 state.fileId num) }
            (Ok num, IsEmpty) -> { state & nextBlock: IsAFile, output: List.append state.output (Empty2 num) }
            _ -> crash "whoops"
    walked.output

compactDisk2 = \disk ->
    compactDisk2Help disk []

compactDisk2Help = \disk, processed ->
    when disk is
        [] -> processed
        [.. as rest, Empty2 size] -> compactDisk2Help rest (List.prepend processed (Empty2 size))
        [.. as rest, File2 fileId fileSize] ->
            emptySpacePointerMaybe = List.findFirstIndex rest \block ->
                when block is
                    Empty2 size if size >= fileSize -> Bool.true
                    _ -> Bool.false
            when emptySpacePointerMaybe is
                Ok emptySpacePointer ->
                    when List.get rest emptySpacePointer is
                        Ok (Empty2 size) ->
                            newDisk = List.join [
                                List.takeFirst rest emptySpacePointer,
                                [File2 fileId fileSize],
                                List.dropFirst rest (emptySpacePointer + 1) |> prependEmpty2 (size - fileSize),
                            ]
                            compactDisk2Help newDisk (prependEmpty2 processed (fileSize))

                        _ -> crash "how did this happen?"

                Err NotFound ->
                    compactDisk2Help rest (List.prepend processed (File2 fileId fileSize))
prependEmpty2 = \processed, size ->
    if size == 0 then
        processed
    else
        when processed is
            [Empty2 size2, .. as rest] -> List.prepend rest (Empty2 (size + size2))
            _ -> List.prepend processed (Empty2 size)
normalize2 = \disk ->
    List.joinMap disk \block ->
        when block is
            File2 fileId fileSize -> List.repeat (File fileId) fileSize
            Empty2 size -> List.repeat Empty size
