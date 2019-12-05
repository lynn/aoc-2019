-- https://adventofcode.com/2019/day/5 in Elm.
-- Paste it in here: https://elm-lang.org/examples/hello

module Main exposing (..)

import Html
import Dict exposing (Dict, get)
import Result exposing (andThen)

-- Definitions

type alias Program = Dict Int Int
type alias Input = List Int
type alias Output = List Int

type Mode = Position | Immediate

decodeMode : Int -> Result String Mode
decodeMode n =
  case n of
    0 -> Ok Position
    1 -> Ok Immediate
    _ -> Err ("unknown mode " ++ String.fromInt n)

type Opcode
  = Arithmetic (Int -> Int -> Int)
  | JumpIf (Int -> Bool)
  | Scan
  | Print
  | Halt

decodeOpcode : Int -> Result String Opcode
decodeOpcode n =
  case n of
    1 -> Ok (Arithmetic (+))
    2 -> Ok (Arithmetic (*))
    3 -> Ok Scan
    4 -> Ok Print
    5 -> Ok (JumpIf (\x -> x /= 0))
    6 -> Ok (JumpIf (\x -> x == 0))
    7 -> Ok (Arithmetic (\x y -> if x < y  then 1 else 0))
    8 -> Ok (Arithmetic (\x y -> if x == y then 1 else 0))
    99 -> Ok Halt
    _ -> Err ("unknown opcode " ++ String.fromInt n)

-- Running a program

run : Program -> Input -> Result String Output
run program input = runFrom 0 program input

runFrom : Int -> Program -> Input -> Result String Output
runFrom i program input =
  let
    word : Int -> Result String Int
    word k = get k program |> Result.fromMaybe "out of program bounds"

    fetch : Mode -> Int -> Result String Int
    fetch mode k =
      case mode of
        Position -> word k
        Immediate -> Ok k

    fetchIndex : Mode -> Int -> Result String Int
    fetchIndex mode k =
      case mode of
        Position -> Ok k
        Immediate -> Err "can't store into immediate"
  in
    -- I really wish Elm had `do`-notation...
    word i |> andThen (\op ->
    decodeMode (modBy 10 (op // 100)) |> andThen (\modeA ->
    decodeMode (modBy 10 (op // 1000)) |> andThen (\modeB ->
    decodeMode (modBy 10 (op // 10000)) |> andThen (\modeC ->
    decodeOpcode (modBy 100 op) |> andThen (\opcode ->
      case opcode of
        Arithmetic f ->
          word (i + 1) |> andThen (fetch modeA) |> andThen (\a ->
          word (i + 2) |> andThen (fetch modeB) |> andThen (\b ->
          word (i + 3) |> andThen (fetchIndex modeC) |> andThen (\c ->
            runFrom (i + 4) (Dict.insert c (f a b) program) input
          )))
        JumpIf f ->
          word (i + 1) |> andThen (fetch modeA) |> andThen (\a ->
          word (i + 2) |> andThen (fetch modeB) |> andThen (\b ->
            runFrom (if f a then b else i + 3) program input
          ))
        Scan ->
          word (i + 1) |> andThen (\pos ->
            case input of
              (x :: xs) -> runFrom (i + 2) (Dict.insert pos x program) xs
              [] -> Err "end of input"
          )
        Print ->
          word (i + 1) |> andThen (fetch modeA) |> andThen (\o ->
            Result.map (\os -> o :: os) (runFrom (i + 2) program input)
          )
        Halt -> Ok []
    )))))
    |> Result.mapError (\e -> "at " ++ String.fromInt i ++ ": " ++ e)

-- Displaying output

loadProgram : List Int -> Program
loadProgram program =
  Dict.fromList (List.indexedMap Tuple.pair program)

showOutput : Output -> String
showOutput out =
  case out of
    [] -> "(nothing)"
    [x] -> String.fromInt x
    (x :: xs) -> String.fromInt x ++ ", " ++ showOutput xs

showRun : Program -> Input -> String
showRun program input =
  showProgramResult (run program input)

showProgramResult : Result String Output -> String
showProgramResult result =
  case result of
    Ok output -> showOutput output
    Err error -> error

myProgram : Program
myProgram = loadProgram [3,225,1,225,6,6,1100,1,238,225,104,0,1101,65,39,225,2,14,169,224,101,-2340,224,224,4,224,1002,223,8,223,101,7,224,224,1,224,223,223,1001,144,70,224,101,-96,224,224,4,224,1002,223,8,223,1001,224,2,224,1,223,224,223,1101,92,65,225,1102,42,8,225,1002,61,84,224,101,-7728,224,224,4,224,102,8,223,223,1001,224,5,224,1,223,224,223,1102,67,73,224,1001,224,-4891,224,4,224,102,8,223,223,101,4,224,224,1,224,223,223,1102,54,12,225,102,67,114,224,101,-804,224,224,4,224,102,8,223,223,1001,224,3,224,1,224,223,223,1101,19,79,225,1101,62,26,225,101,57,139,224,1001,224,-76,224,4,224,1002,223,8,223,1001,224,2,224,1,224,223,223,1102,60,47,225,1101,20,62,225,1101,47,44,224,1001,224,-91,224,4,224,1002,223,8,223,101,2,224,224,1,224,223,223,1,66,174,224,101,-70,224,224,4,224,102,8,223,223,1001,224,6,224,1,223,224,223,4,223,99,0,0,0,677,0,0,0,0,0,0,0,0,0,0,0,1105,0,99999,1105,227,247,1105,1,99999,1005,227,99999,1005,0,256,1105,1,99999,1106,227,99999,1106,0,265,1105,1,99999,1006,0,99999,1006,227,274,1105,1,99999,1105,1,280,1105,1,99999,1,225,225,225,1101,294,0,0,105,1,0,1105,1,99999,1106,0,300,1105,1,99999,1,225,225,225,1101,314,0,0,106,0,0,1105,1,99999,108,226,226,224,102,2,223,223,1005,224,329,101,1,223,223,1107,226,677,224,1002,223,2,223,1005,224,344,101,1,223,223,8,226,677,224,102,2,223,223,1006,224,359,101,1,223,223,108,677,677,224,1002,223,2,223,1005,224,374,1001,223,1,223,1108,226,677,224,1002,223,2,223,1005,224,389,101,1,223,223,1007,677,677,224,1002,223,2,223,1006,224,404,1001,223,1,223,1108,677,677,224,102,2,223,223,1006,224,419,1001,223,1,223,1008,226,677,224,102,2,223,223,1005,224,434,101,1,223,223,107,677,677,224,102,2,223,223,1006,224,449,1001,223,1,223,1007,226,677,224,102,2,223,223,1005,224,464,101,1,223,223,7,677,226,224,102,2,223,223,1005,224,479,101,1,223,223,1007,226,226,224,102,2,223,223,1005,224,494,101,1,223,223,7,677,677,224,102,2,223,223,1006,224,509,101,1,223,223,1008,677,677,224,1002,223,2,223,1006,224,524,1001,223,1,223,108,226,677,224,1002,223,2,223,1006,224,539,101,1,223,223,8,226,226,224,102,2,223,223,1006,224,554,101,1,223,223,8,677,226,224,102,2,223,223,1005,224,569,1001,223,1,223,1108,677,226,224,1002,223,2,223,1006,224,584,101,1,223,223,1107,677,226,224,1002,223,2,223,1005,224,599,101,1,223,223,107,226,226,224,102,2,223,223,1006,224,614,1001,223,1,223,7,226,677,224,102,2,223,223,1005,224,629,1001,223,1,223,107,677,226,224,1002,223,2,223,1005,224,644,1001,223,1,223,1107,677,677,224,102,2,223,223,1006,224,659,101,1,223,223,1008,226,226,224,1002,223,2,223,1006,224,674,1001,223,1,223,4,223,99,226]

main =
  Html.section []
    [ Html.ul []
      [ Html.li [] [Html.text (showRun myProgram [1])]
      , Html.li [] [Html.text (showRun myProgram [5])]
      ]
    ]
