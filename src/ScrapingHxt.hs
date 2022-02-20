{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module ScrapingHxt where
import Data.Char
import Data.List (isPrefixOf)
import Data.Maybe (listToMaybe, mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy.Char8 as BL
import Text.XML.HXT.Core
import Text.XML.HXT.CSS

import Control.Arrow
import Data.Tree.NTree.TypeDefs

import Data.Aeson
import Data.Aeson.Types (parse, Result(..))
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM 
import DownloadHtml
import Model



parseHTML = readString 
  [ withValidate no,
    withParseHTML yes,
    withWarnings no
  ]

scraping body parser = runX (parseHTML body >>> parser)


atTagCase tag = deep (isElem >>> hasNameWith ((== tag') . upper . localPart))
  where tag' = upper tag
        upper = map toUpper

onTd = proc r -> do
  td <- atTagCase "td" -< r
  (getText <<< deep isText) <<< arr 
     (\t@(NTree a xs) -> 
       if null xs 
         then NTree a [NTree (XText "null") []] 
         else t
     ) -< td

extract =
  atTagCase "tr"
  >>> proc r -> do
    xs <- listA onTd -< r
    returnA -< xs

getStrings :: String -> IO [[String]]
getStrings query_path = getHtml query_path >>= fmap tail . flip scraping extract >>= pure . map 
  (map (\case "t" -> "true"; "f" -> "false"; s -> let t = dropWhile (== '-') s in  if (and (map isDigit t) || s == "null") then s else "\"" ++ s ++ "\""))

convertValues :: [T.Text] -> [String] -> Value
convertValues ks xs =
    let values = mapMaybe (decodeStrict . T.encodeUtf8 . T.pack) xs
    in Object $ HM.fromList $ zip ks values

-- @ (query_path, fields) -> IO Json 
convertJson :: (String, [T.Text]) -> IO Value
convertJson (path, keys) = Array . V.fromList . map (convertValues keys) <$> getStrings path 

brandlistFields = ("sql/scraping/brandlistAll.sql", 
  [  
    "id",
    "brandname", 
    "brandfurigana",
    "url",
    "kind",
    "lost",
    "median",
    "twitter"
  ])

createrlistFields = ("sql/scraping/createrlistAll.sql",
  [
    "id",
    "name",
    "furigana",
    "title",
    "url",
    "circle",
    "twitter_username",
    "blog",
    "pixiv",
    "blog_title"
  ])

gamelistFields = ("sql/scraping/gamelistAll.sql",
 [
    "id", 
    "gamename", 
    "furigana", 
    "sellday",
    "brandname",
    "median",
    "stdev",
    "count2",
    "shoukai",
    "model", 
    "erogame"
 ])


outputJsonToFile :: (String, [T.Text]) -> String -> IO ()
outputJsonToFile set output_path = convertJson set >>= BL.writeFile output_path . encode

getResults :: FromJSON a => (String, [T.Text]) -> IO (V.Vector (Result a))
getResults set = (\(Array vs) -> fmap (parse parseJSON) vs) <$> convertJson set
 
catResultsIO :: V.Vector (Result a) -> IO [a]
catResultsIO = loop . V.toList
  where
    loop ((Error s): xs) = putStrLn s >> loop xs
    loop ((Success a) : xs) = (a:) <$> loop xs
    loop _ =  pure []

getResultsWithoutError :: FromJSON a => (String, [T.Text]) -> IO [a]
getResultsWithoutError set = getResults set >>= catResultsIO

getBrandList :: IO [Brand]
getBrandList = getResultsWithoutError brandlistFields

getCreaterList :: IO [Creater]
getCreaterList = getResultsWithoutError createrlistFields

getGameList :: IO [Game]
getGameList = getResultsWithoutError gamelistFields
