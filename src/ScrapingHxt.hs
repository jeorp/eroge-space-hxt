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
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM 
import DownloadHtml



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
  a <- atTagCase "td" -< r
  (getText <<< deep isText) <<< arr 
     (\t@(NTree a xs) -> 
       if null xs 
         then NTree a [NTree (XText "null") []] 
         else t
     ) -< a

extract =
  atTagCase "tr"
  >>> proc r -> do
    xs <- listA onTd -< r
    returnA -< xs

getStrings :: String -> IO [[String]]
getStrings query_path = getHtml query_path >>= fmap tail . flip scraping extract >>= pure . map (map (\case "t" -> "true"; "f" -> "false"; s -> if (and (map isDigit s) || s == "null") then s else "\"" ++ s ++ "\""))

convertValues :: [T.Text] -> [String] -> Value
convertValues ks xs =
    let values = mapMaybe (decodeStrict . T.encodeUtf8 . T.pack) xs
    in Object $ HM.fromList $ zip ks values

-- @ (query path, fields) -> Json 
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

outputJsonToFile :: (String, [T.Text]) -> String -> IO ()
outputJsonToFile set output_path = convertJson set >>= BL.writeFile output_path . encode