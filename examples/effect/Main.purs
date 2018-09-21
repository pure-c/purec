module Main where

import Prelude

import Effect (Effect)
import Effect.Console as Console
import Effect.Uncurried

fn1 :: EffectFn1 String String
fn1 =
  mkEffectFn1 \a ->
    pure $ ">" <> a <> "<"

fn2 :: EffectFn2 String String String
fn2 =
  mkEffectFn2 \a b ->
    pure $ ">" <> a <> ":" <> b <> "<"

fn3 :: EffectFn3 String String String String
fn3 =
  mkEffectFn3 \a b c ->
    pure $ ">" <> a <> ":" <> b <> ":" <> c <> "<"

fn4 :: EffectFn4 String String String String String
fn4 =
  mkEffectFn4 \a b c d ->
    pure $ ">" <> a <> ":" <> b <> ":" <> c <> ":" <> d <> "<"

fn5 :: EffectFn5 String String String String String String
fn5 =
  mkEffectFn5 \a b c d e ->
    pure $ ">" <> a <> ":" <> b <> ":" <> c <> ":" <> d <> ":" <> e <> "<"

fn6 :: EffectFn6 String String String String String String String
fn6 =
  mkEffectFn6 \a b c d e f ->
    pure $ ">" <> a <> ":" <> b <> ":" <> c <> ":" <> d <> ":" <> e <> ":" <> f <> "<"

fn7 :: EffectFn7 String String String String String String String String
fn7 =
  mkEffectFn7 \a b c d e f g ->
    pure $ ">" <> a <> ":" <> b <> ":" <> c <> ":" <> d <> ":" <> e <> ":" <> f <> ":" <> g <> "<"

fn8 :: EffectFn8 String String String String String String String String String
fn8 =
  mkEffectFn8 \a b c d e f g h ->
    pure $ ">" <> a <> ":" <> b <> ":" <> c <> ":" <> d <> ":" <> e <> ":" <> f <> ":" <> g <> ":" <> h <> "<"

fn9 :: EffectFn9 String String String String String String String String String String
fn9 =
  mkEffectFn9 \a b c d e f g h i ->
    pure $ ">" <> a <> ":" <> b <> ":" <> c <> ":" <> d <> ":" <> e <> ":" <> f <> ":" <> g <> ":" <> h <> ":" <> i <> "<"

fn10 :: EffectFn10 String String String String String String String String String String String
fn10 =
  mkEffectFn10 \a b c d e f g h i j ->
    pure $ ">" <> a <> ":" <> b <> ":" <> c <> ":" <> d <> ":" <> e <> ":" <> f <> ":" <> g <> ":" <> h <> ":" <> i <> ":" <> j <> "<"

main :: Effect Unit
main = do
  Console.log =<< runEffectFn1  fn1  "a"
  Console.log =<< runEffectFn2  fn2  "a" "b"
  Console.log =<< runEffectFn3  fn3  "a" "b" "c"
  Console.log =<< runEffectFn4  fn4  "a" "b" "c" "d"
  Console.log =<< runEffectFn5  fn5  "a" "b" "c" "d" "e"
  Console.log =<< runEffectFn6  fn6  "a" "b" "c" "d" "e" "f"
  Console.log =<< runEffectFn7  fn7  "a" "b" "c" "d" "e" "f" "g"
  Console.log =<< runEffectFn8  fn8  "a" "b" "c" "d" "e" "f" "g" "h"
  Console.log =<< runEffectFn9  fn9  "a" "b" "c" "d" "e" "f" "g" "h" "i"
  Console.log =<< runEffectFn10 fn10 "a" "b" "c" "d" "e" "f" "g" "h" "i" "j"
