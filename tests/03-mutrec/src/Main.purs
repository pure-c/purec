module Main where

data Unit = Unit
type Effect a = Unit -> a

main :: Effect Int
main _ =
  let
    f =
      case _ of
        [0] ->
          g [1]
        [1] ->
          g [2]
        [2] ->
          g [3]
        [3] ->
          [1] -- 'g' should finish!
        _ ->
          [1]
    g =
      case _ of
        [0] ->
          f [1]
        [1] ->
          f [2]
        [2] ->
          f [3]
        [3] ->
          [0]
        _ ->
          [1]
  in case f [0] of
    [0] -> 0
    _   -> 1
