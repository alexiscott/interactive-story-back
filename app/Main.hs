{-# LANGUAGE OverloadedStrings #-}
module Main where

import Text.Parsec
import Text.Parsec.String (Parser)
import Data.List (intercalate)
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as B
import Data.Aeson (encode, object, (.=), Value)
import Data.Aeson.Key (fromString)
import Data.Aeson.KeyMap (KeyMap)
import Text.Read (readMaybe)

data Choice = Choice {
    choiceText :: String,
    choiceSvg :: String,
    nextPart :: Maybe Int
} deriving (Show)

data Part = Part {
    partText :: String,
    choices :: [Choice]
} deriving (Show)

type Story = M.Map Int Part

csvFile :: Parser [[String]]
csvFile = endBy line (char '\n')

line :: Parser [String]
line = sepBy cell (char ',')

cell :: Parser String
cell = many (noneOf ",\n")

parseCSV :: String -> Either ParseError [[String]]
parseCSV = parse csvFile "(unknown)"

convertToStory :: [[String]] -> Story
convertToStory rows = foldr addLine M.empty (drop 1 rows)

-- Test data.
-- l1 = ["1","Some text","", "", ""]
-- l2 = ["1","Some text and more","A", "B", "C"]

addLine :: [String] -> Story -> Story
addLine (x:text:xs) story = case readMaybe x :: Maybe Int of
    Nothing -> error "Invalid key format"
    Just id ->
        if concat xs == []
        then M.insert id (Part text []) story -- At end, so empty no choices.
        else M.insertWith addChoice id (Part text [choice]) story
        where choiceText = head xs
              choiceSvg = head (tail xs)
              nextPart = readMaybe (head (tail (tail xs))) :: Maybe Int
              choice = Choice choiceText choiceSvg nextPart
  
addChoice :: Part -> Part -> Part
addChoice (Part t1 c1) (Part _ c2) = Part t1 (c1 ++ c2)

storyToJSON :: Story -> Value
storyToJSON story = object ["parts" .= partsObject]
  where
    partsObject = object $ map partToJSON (M.toList story)
    partToJSON (partNum, Part text choices) =
     fromString (show partNum) .= object ["text" .= text, "choices" .= map choiceToJSON choices]

    choiceToJSON (Choice choiceText choiceSvg nextPart) =
        object ["text" .= choiceText, "svg" .= choiceSvg, "nextPart" .= nextPart]

main :: IO ()
main = do
    csvData <- readFile "story.csv"
    case parseCSV csvData of
        Left err -> print err
        Right rows -> do
            let story = convertToStory rows
            B.writeFile "../interactive-story-front/story.json" (encode $ storyToJSON story)
            putStrLn "story.json generated successfully."


