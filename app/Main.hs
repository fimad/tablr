module Main where

import Control.Applicative ((<$>), (<*>))
import Data.List (intercalate, isPrefixOf)
import Data.Maybe (fromMaybe)
import System.Environment (getArgs)

type Table = [Row]
type Cell  = String
data Row   = Cells [Cell] | Divider

main :: IO ()
main = do
    args  <- getArgs
    input <- getContents
    putStr $ parseArgs args input

parseArgs :: [String] -> String -> String
parseArgs []     input | length input == 0
                       || all (== '-') input = "\n"
                       | otherwise           = createTable input
parseArgs ["-r"] []    = "\n"
parseArgs ["-r"] input = reverseTable input
parseArgs _      _     = usage

usage :: String
usage = unlines [
        "tablr    < table.txt       > fancy-table.txt"
    ,   "tablr -r < fancy-table.txt > table.txt"
    ]

createTable :: String -> String
createTable = asciiFromTable
            . addWhitespace
            . sanitize
            . tableFromSpec

split :: Eq a => a -> [a] -> [[a]]
split _ []                 = [[]]
split a (x:xs) | a == x    = [] : split a xs
               | otherwise = let (y:ys) = split a xs
                             in  (x : y) : ys

mapCells :: ([Cell] -> [Cell]) -> Table -> Table
mapCells _ []               = []
mapCells f (Divider:xs)     = Divider : mapCells f xs
mapCells f (Cells cells:xs) = Cells (f cells) : mapCells f xs

mapCells' :: ([Cell] -> a) -> Table -> [a]
mapCells' _ []               = []
mapCells' f (Divider:xs)     = mapCells' f xs
mapCells' f (Cells cells:xs) = (f cells) : mapCells' f xs

tableFromSpec :: String -> Table
tableFromSpec = map rowFromString . lines

rowFromString :: String -> Row
rowFromString row | all (== '-') row = Divider
                  | otherwise        = Cells $ split '|' row

padWith :: a -> Int -> [a] -> [a]
padWith a maxLength xs = xs ++ (replicate (maxLength - length xs) a)

sanitize :: Table -> Table
sanitize table = let maxColumns = maximum $ mapCells' length table
                  in mapCells (padWith "" maxColumns) table

stripWhitespace :: String -> String
stripWhitespace = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')

addWhitespace :: Table -> Table
addWhitespace table = let stripped     = mapCells (map stripWhitespace) table
                          columnCounts = mapCells' (map length) stripped
                          maxes        = foldr1 (zipWith max) columnCounts
                          padRow       = zipWith ($) (map (padWith ' ') maxes)
                       in mapCells padRow stripped

headCellRow :: Table -> [Cell]
headCellRow []               = error "There must be at least one row."
headCellRow (Divider:xs)     = headCellRow xs
headCellRow (Cells cells:xs) = cells

asciiFromTable :: Table -> String
asciiFromTable table = let asciiRows = map asciiFromRow table
                        in unlines $ divider : asciiRows ++ [divider]
    where
        divider = (++ "-+") . ("+-" ++)
                $ intercalate "-+-"
                $ map (flip replicate '-')
                $ map length (headCellRow table)

        asciiFromRow Divider       = divider
        asciiFromRow (Cells cells) = (++ " |")
                                   $ ("| " ++)
                                   $ intercalate " | " cells

reverseTable :: String -> String
reverseTable = unlines
             . map stripWhitespace
             . map dropLeadingTrailing  -- Drop outside bars
             . map replaceDividers
             . dropLeadingTrailing      -- Drop headrs
             . lines

dropLeadingTrailing :: [a] -> [a]
dropLeadingTrailing = reverse . drop 1 . reverse . drop 1

replaceDividers :: String -> String
replaceDividers x | "+-" `isPrefixOf` x = "---"
                  | otherwise           = x
