module Data.Clone where

class Cloneable a where
  clone :: a -> a
  cloneWOrg :: a -> String -> a
