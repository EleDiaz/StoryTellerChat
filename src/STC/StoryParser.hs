{-# LANGUAGE OverloadedStrings #-}
module STC.StoryParser
    ( author
    , title
    , commentary
    , storyFrag
    , storyChoices
    , story
    , optionsParser
    ) where

import           Control.Applicative
import           Data.Attoparsec.Text as P
import           Data.Functor
import qualified Data.Text            as T

import           STC.StoryFlow

author :: Parser T.Text
author = do
  char '#'
  author <- many1' letter `sepBy'` (many1' $ char ' ')
  char '#'
  return . T.pack $ unwords author

title :: Parser T.Text
title = do
  char '{'
  title <- many1' letter `sepBy'` (many1' $ char ' ')
  char '}'
  return . T.pack $ unwords title

commentary :: Parser ()
commentary = do
  many' $ char ' '
  string "\\\\"
  skipWhile (/= '\n')
  return ()

storyFrag :: Parser (Int, T.Text, [(T.Text, Int)])
storyFrag = do
  many' $ char ' '
  char '<'
  identifier <- many1' digit
  char '>'
  many' $ char ' '
  text <- scan 0 (\st ch -> case (st,ch) of -- Take text until find two \n\n follow
    (0,'\n') -> Just 1
    (1,'\n') -> Nothing
    (x,' ') -> Just x
    (_,_) -> Just 0
    )
  skipSpace
  options <- storyChoices
  return (read identifier, T.unwords $ T.words text, options)

storyChoices :: Parser [(T.Text, Int)]
storyChoices = many' $ do
  many' $ char ' '
  char '-' <|> char '~'
  many' $ char ' '
  text <- P.takeWhile (/= '@')
  char '@'
  digits <- many1' digit
  many' $ char ' '
  endOfLine
  return (T.strip text, read digits)

story :: Parser (T.Text, T.Text, [(Int, T.Text, [(T.Text, Int)])])
story = do
  many' space
  aut <- author
  many' space
  tit <- title
  many' space
  frags <- many1' storyFrag
  return (aut,tit,frags)


optionsParser :: Parser Options
optionsParser = choice [Choice <$> digits, Start <$ start, Error <$> takeText]
  where
    digits = read <$> many1' digit
    start = string "start" <|> string "/start"
