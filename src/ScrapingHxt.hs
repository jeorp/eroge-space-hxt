{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}

module ScrapingHxt where
import Data.Char
import Data.List (isPrefixOf)
import Data.Maybe (listToMaybe)
import Text.XML.HXT.Core
import Text.XML.HXT.CSS
import Control.Arrow



parseHTML = readString 
  [ withValidate no,
    withParseHTML yes,
    withWarnings no
  ]

scraping body parser = runX (parseHTML body >>> parser)


atTagCase tag = deep (isElem >>> hasNameWith ((== tag') . upper . localPart))
  where tag' = upper tag
        upper = map toUpper