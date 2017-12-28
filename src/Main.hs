-- {-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.ByteString.Lazy.UTF8 (toString)
import Network.HTTP.Simple
import Text.HTML.TagSoup
import Data.Text.Lazy (replace, pack, unpack)
import Data.List
import System.IO

type Tags = [Tag String]
type Link = String

baseUrl = "http://bars.org.ru"

main :: IO ()
main = do
  lettersLinks <- getLettersLinks
  file <- openFile "dictionary.txt" WriteMode
  scrapLetterLinks lettersLinks file
  hClose file

openURL :: String -> IO Tags
openURL url = do
  request <- parseRequest url
  response <- httpLBS request
  return $ parseTags $ toString $ getResponseBody response

getLettersLinks :: IO [Link]
getLettersLinks = do
  responseBody <- openURL $ baseUrl ++ "/articles"
  return $ parseLettersLinks responseBody

parseLettersLinks :: Tags -> [Link]
parseLettersLinks tags =
  map (fromAttrib "href")
  $ filter (~== "<a>")
  $ takeWhile (~/= "</ul>")
  $ dropWhile (~/= "<ul class=\"nav nav-pills\">")
  $ tags

scrapLetterLinks :: [Link] -> Handle -> IO ()
scrapLetterLinks [] _ = return ()
scrapLetterLinks (letterLink:xs) file = do
  scrapLetterPages letterLink file
  scrapLetterLinks xs file

scrapLetterPages :: Link -> Handle -> IO ()
scrapLetterPages pageLink file = do
  tags <- openURL $ baseUrl ++ pageLink
  mapM (hPutStrLn file) $ parsePage tags
  let maybeNextLink = parseNextPageLink tags
  case maybeNextLink of
    Nothing -> return ()
    Just nextLink -> scrapLetterPages nextLink file

parsePage :: Tags -> [String]
parsePage tags =
  let
    rows =
      partitions (~== "<tr>")
      $ takeWhile (~/= "</tbody>")
      $ dropWhile (~/= "<tbody>")
      $ tags
  in
    map (parseRow . partitions (~== "<td>"))
    $ filter (not . null . tail) rows

parseTexts :: Tags -> [String]
parseTexts tags = map fromTagText $ filter isTagText tags

parseRow :: [Tags] -> String
parseRow row =
  let
    arabicWord = head $ parseTexts $ head row
    plural = head $ parseTexts $ row !! 3
  in
    intercalate "--|--"
    $ map (unwords . words . (replace' "\n" ""))
    $ [ arabicWord
      , parsePresentForm row
      , plural
      , parseTranslation row
      ]

parsePresentForm :: [Tags] -> String
parsePresentForm row =
  let
    string = case parseTexts $ row !! 2 of
                [] -> ""
                x:_ -> x
    forms = [('а', 'a'), ('и', 'i'), ('у', 'u')]
  in
    intersperse ','
    $ map snd
    $ filter ((`elem` string) . fst) forms

parseTranslation :: [Tags] -> String
parseTranslation row =
  unlines
  $ filter (not . null)
  $ map (unwords . words)
  $ lines
  $ concat
  $ parseTexts
  $ row !! 5

init' :: [a] -> [a]
init' [] = []
init' xs = init xs

replace' :: String -> String -> String -> String
replace' needle replacement haystack =
  unpack $ replace (pack needle) (pack replacement) $ pack haystack

parseNextPageLink :: Tags -> Maybe Link
parseNextPageLink responseBody =
  let
    links =
      filter (~== "<a>")
      $ takeWhile (~/= "</li>")
      $ dropWhile (~/= "<li class=next>")
      $ takeWhile (~/= "</ul>")
      $ dropWhile (~/= "<ul class=pagination>")
      $ responseBody
  in
    case links of
      [] -> Nothing
      link:_ -> Just $ fromAttrib "href" link
