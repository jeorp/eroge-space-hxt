{-# LANGUAGE OverloadedStrings #-}
module Postdata where

newtype PostSql = PostSql {sql :: String} deriving (Show)

