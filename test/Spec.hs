{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Main where

import           Data.Attoparsec.Text
import           Data.List
import qualified Data.Text            as T
import           Test.Hspec
import           Text.RawString.QQ

import           STC
import           STC.StoryFlow
import           STC.StoryParser


main :: IO ()
main = hspec $ do
  -- describe "BotServer" $ do
    -- it "not exist telegramBotServer" $ do
      -- telegramBotServer "" `shouldThrow` anyException

  describe "StoryParser" $ do
    it "Parse Author" $ do
      parseOnly author "#Eleazar Diaz Delgado#" `shouldBe` Right "Eleazar Diaz Delgado"

      parseOnly author "#Eleazar  Diaz   Delgado#" `shouldBe` Right "Eleazar Diaz Delgado"

      parseOnly author "#Eleazar Díaz Delgado#" `shouldBe` Right "Eleazar Díaz Delgado"

    it "Parse title" $ do
      parseOnly title "{Mi título de historia}" `shouldBe` Right "Mi título de historia"

      parseOnly title "{Mi    título de    historia}" `shouldBe` Right "Mi título de historia"

    it "Parse Commentary - to ignore" $ do
      parseOnly commentary "\\\\ Esto es un comentario \n" `shouldBe` Right ()

      parseOnly commentary "  \\\\ comentario \n" `shouldBe` Right ()

    it "Parse Story Fragment" $ do
      parseOnly storyFrag (T.intercalate "\n"
        [ "<0> Esta es la historia de"
        , "  alguien, "
        , "  someone"]) `shouldBe` Right (0,"Esta es la historia de alguien, someone",[])

      parseOnly storyFrag (T.intercalate "\n"
        [ "<0> Esta es la historia de"
        , "  alguien, "
        , "someone"
        , ""
        , "- Mi opción primera @1  "
        , "  ~ Mi segunda opción @2"
        , "-Mi tercera opción @1  \n"])
        `shouldBe`
          Right (0,"Esta es la historia de alguien, someone",
            [ ("Mi opción primera", 1)
            , ("Mi segunda opción", 2)
            , ("Mi tercera opción", 1)])

    it "Parse choices from a fragment" $ do
      parseOnly storyChoices (T.intercalate "\n"
        [ "- Mi opción primera @1  "
        , "  ~ Mi segunda opción @2"
        , "-Mi tercera opción @1  \n"])
      `shouldBe`
        Right
          [ ("Mi opción primera", 1)
          , ("Mi segunda opción", 2)
          , ("Mi tercera opción", 1)]

    it "Parse a full story" $ do
      let storyText = [r|#Autor#
{Nombre Historia}

<0> Esta es la descripción de la escena 0
    Esto sigue siéndolo, no cambiará hasta
    que aparezca un caracter virgulilla, una
    arroba, un símbolo de porentaje u otra escena

<1> Esta es la descripción de la escena 1

    ~opción A @2
    ~opción B @3
|]
      parseOnly story storyText `shouldBe` Right
        ("Autor"
        ,"Nombre Historia"
        , [(0,"Esta es la descripción de la escena 0 Esto sigue siéndolo, no\
              \ cambiará hasta que aparezca un caracter virgulilla, una arroba,\
              \ un símbolo de porentaje u otra escena",[])
          ,(1,"Esta es la descripción de la escena 1",[("opción A",2),("opción B",3)])])

    it "Parse options from user input" $ do
      parseOnly optionsParser "start" `shouldBe` Right Start
      parseOnly optionsParser "/start" `shouldBe` Right Start

      parseOnly optionsParser "45612" `shouldBe` Right (Choice 45612)
      parseOnly optionsParser "-456" `shouldBe` Right (Error "-456")

      parseOnly optionsParser "another thing not related" `shouldBe`
        Right (Error "another thing not related")
