module Main where

data Unit = Unit
type Effect a = Unit -> a

data Step
  = Start
  | Step1
  | Step2
  | Done

main :: Effect Int
main _ =
  let
    f =
      case _ of
        Start ->
          g Step1
        Step1 ->
          g Step2
        Step2 ->
          g Done
        Done ->
          1 -- 'g' should finish!
    g =
      case _ of
        Start ->
          f Step1
        Step1 ->
          f Step2
        Step2 ->
          f Done
        Done ->
          0
  in f Start
