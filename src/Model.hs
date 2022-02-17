{-# LANGUAGE TemplateHaskell, DataKinds, FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Model where 

import Data.Extensible
import Data.Text
import Data.Proxy
import GHC.TypeLits
import Database.SQLite.Simple.FromRow (field, FromRow(..)) 
import Database.SQLite.Simple.FromField (FromField)

instance Forall (KeyTargetAre KnownSymbol FromField) xs => FromRow (Record xs) where
  fromRow =  hgenerateFor (Proxy @ (KeyTargetAre KnownSymbol FromField)) (const $ Field . pure <$> field)

type BrandList = Record 
 '[
    "id" :> Int,
    "brandname" :> Text, 
    "brandfurigana" :> Text,
    "url" :> Maybe Text,
    "kind" :> Text,
    "lost" :> Bool,
    "directlink" :> Maybe Text,
    "median" :> Maybe Text,
    "twitter" :> Maybe Text
  ]