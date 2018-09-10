module POSIX.GetOpt
  ( newBasicParser
  , getopt
  , argv
  , optind
  , BasicParser
  , OptString
  , Argv
  , OptInd
  , GetOptRet
  ) where

import Prelude

import Data.Maybe (Maybe, isJust)
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn3, runEffectFn1, runEffectFn3)
import Foreign (Foreign)

foreign import data BasicParser :: Type

type OptString = String
type Argv = Array String
type OptInd = Int

newBasicParser
  :: OptString
  -> Argv
  -> OptInd
  -> Effect BasicParser
newBasicParser =
  runEffectFn3 newBasicParserImpl

foreign import newBasicParserImpl
  :: EffectFn3
      OptString
      Argv
      OptInd
      BasicParser

type GetOptRet =
  Maybe
    { option :: Char
    , optarg :: Maybe String
    , optopt :: Maybe Char
    , error :: Boolean
    }

getopt
  :: BasicParser
  -> Effect GetOptRet
getopt parser = ado
  x <- runEffectFn1 getoptImpl parser
  in
    Nullable.toMaybe x <#> \{ option, optarg, optopt, error } ->
      { option
      , optarg: Nullable.toMaybe optarg
      , optopt: Nullable.toMaybe optopt
      , error: isJust $ Nullable.toMaybe error
      }

type GetOptRetImpl =
  Nullable
    { option :: Char
    , optarg :: Nullable String
    , optopt :: Nullable Char
    , error :: Nullable Foreign
    }

foreign import getoptImpl
  :: EffectFn1
      BasicParser
      GetOptRetImpl

optind :: BasicParser -> Effect OptInd
optind = runEffectFn1 optindImpl

foreign import optindImpl
  :: EffectFn1
      BasicParser
      OptInd

argv :: BasicParser -> Effect Argv
argv = runEffectFn1 argvImpl

foreign import argvImpl
  :: EffectFn1
      BasicParser
      Argv
