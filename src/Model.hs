{-# LANGUAGE TemplateHaskell, DataKinds, FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Model where 

import Data.Extensible
import Data.Text
import Data.Aeson
import Data.Proxy
import GHC.TypeLits
import Database.SQLite.Simple.FromRow (field, FromRow(..)) 
import Database.SQLite.Simple.FromField (FromField)

instance Forall (KeyTargetAre KnownSymbol FromField) xs => FromRow (Record xs) where
  fromRow =  hgenerateFor (Proxy @ (KeyTargetAre KnownSymbol FromField)) (const $ Field . pure <$> field)

type Brand = Record 
 '[
    "id" :> Integer,
    "brandname" :> Maybe Text, 
    "brandfurigana" :> Maybe Text,
    "url" :> Maybe Text,
    "kind" :> Text,
    "lost" :> Maybe Bool,
    "median" :> Maybe Int,
    "twitter" :> Maybe Text
  ]

type Creater = Record 
 '[
    "id" :> Integer ,
    "name" :> Text,
    "furigana" :> Text,
    "title" :> Maybe Text,
    "url" :> Maybe Text,
    "circle" :> Maybe Text,
    "twitter_username" :> Maybe Text,
    "blog" :> Maybe Text,
    "pixiv":> Maybe Integer,
    "blog_title" :> Maybe Text
  ]

type Game = Record 
 '[
    "id" :> Integer, 
    "gamename" :> Text, 
    "furigana" :> Maybe Text, 
    "sellday" :> Text,
    "brandname" :> Integer,
    "median" :> Maybe Int,
    "stdev" :> Maybe Int,
    "count2" :> Maybe Int,
    "shoukai" :> Maybe Text,
    "model" :> Maybe Text, 
    "erogame" :> Bool
  ]
