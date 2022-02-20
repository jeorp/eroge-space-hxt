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
getStrings query_path = getHtml query_path >>= fmap tail . flip scraping extract >>= pure . map (map (\case "t" -> "true"; "f" -> "false"; s -> if (and (map isDigit s) || s == "null") then s else "\"" ++ s ++ "\""))

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

myuserviewFields = ("sql/scraping/myuserviewAll.sql",
 [
    "uid",
    "title",
    "url",
    "hitokoto",
    "touhyou",
    "choubun_ress",
    "birth",
    "sex"
 ])

shokushuFields = ("sql/scraping/shokushuAll.sql", 
 [
    "id",
    "game",
    "creater",
    "shubetu",
    "shubetu_detail",
    "shubetu_detail_name",
    "timestamp",
    "sort_num"
 ])

userreviewFields = ("sql/scraping/userreviewAll.sql",
 [
    "game",
    "uid", 
    "tokuten",
    "tourokubi",
    "hitokoto",
    "memo",
    "netabare",
    "giveup",
    "possession",
    "play",
    "reserve",
    "outline",
    "before_hitokoto",
    "before_tokuten",
    "before_tourokubi",
    "display",
    "play_tourokubi",
    "outline_netabare",
    "outline_tourokubi",
    "display_unique_count",
    "sage",
    "before_purchase_will",
    "before_sage",
    "total_play_time",
    "time_before_understanding_fun",
    "okazu_tokuten",
    "trial_version_hitokoto",
    "trial_version_hitokoto_sage",
    "trial_version_hitokoto_tourokubi",
    "timestamp"
 ])

outputJsonToFile :: (String, [T.Text]) -> String -> IO ()
outputJsonToFile set output_path = convertJson set >>= BL.writeFile output_path . encode

