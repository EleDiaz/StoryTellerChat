{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Text (pack, strip)

import           STC

main :: IO ()
main = do
  token <- readFile "telegram.token"
  telegramBotServer . strip $ pack token
