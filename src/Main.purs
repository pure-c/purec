module Main
  ( main
  ) where

import Prelude

import Data.Array as A
import Data.Maybe (Maybe(..))
import Debug.Trace (traceM)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Node.Process as Process
import POSIX.GetOpt.Monad (GetOptT, getopt, runGetOptT)
import POSIX.GetOpt.Monad as GetOpt

type RawOptions =
  { output :: Maybe String
  , input :: Maybe (Array String)
  }

emptyOpts :: RawOptions
emptyOpts =
  { input: Nothing
  , output: Nothing
  }

help = """Usage: purec [-h] [--output <path>] <input>

Options:
  -h, --help
    Show this help and exit
  -o, --output <path>
    Where to write the generated C files.
    Defaults to directory of corefn.json."""

main :: Effect Unit
main = do
  -- parse command line
  rawOpts <-
    let
      go :: RawOptions -> GetOptT Effect RawOptions
      go opts =
        getopt >>= case _ of
          Just { option: 'h' } ->
            liftEffect do
              Console.log help
              Process.exit 0
          Just { option: 'o', optarg: Just arg } ->
            go $ opts { output = Just arg }
          Just _ ->
            go opts
          Nothing -> do
            optind <- GetOpt.optind
            argv   <- GetOpt.argv
            pure $
              opts
                { input = Just $ A.drop optind argv
                }
    in do
      argv <- Process.argv
      runGetOptT "h(help)o:(output)" argv 2 $
        go emptyOpts

  -- validate options
  input <-
    case A.uncons =<< rawOpts.input of
      Just { head: filePath } ->
        pure filePath
      _ ->
        die "missing <input>"

  -- run utility
  traceM { input }

  where
  die msg = do
    Console.error msg
    Process.exit 1
