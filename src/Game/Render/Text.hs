{-# LANGUAGE OverloadedStrings #-}

module Game.Render.Text where

import           Control.Monad.State.Strict hiding (State)
import           Data.Functor.Identity
import qualified Data.HashMap.Strict as HM
import           Data.List
import           Data.Monoid
import qualified Data.Text as T

import           Game.Dialog
import           Game.Item
import           Game.Monad
import           Game.State

indent :: T.Text -> T.Text
indent = ("    " <>)

nameWidth :: Int
nameWidth = 15

numberWidth :: Int
numberWidth = 7

itemNameText :: ItemName -> T.Text
itemNameText = T.justifyLeft nameWidth '.' . unItemName

priceText :: Worth -> T.Text
priceText = T.justifyRight numberWidth '.' . T.pack . show . unMoney . unWorth

quantityText :: Sum Quantity -> T.Text
quantityText = T.justifyRight numberWidth '.' . T.pack . show . unQuantity . getSum

shopText :: [Item Identity] -> Game T.Text
shopText is = do
    i <- askItem
    p <- askPrice
    let header = T.center nameWidth ' ' i <> T.center numberWidth ' ' p
    return $ T.unlines $ (header:) $ map f $ sortOn (runIdentity . iName) is
    where
        f i = indent $ itemNameText (runIdentity $ iName i) <> priceText (iWorth i)

inventoryText :: Game T.Text
inventoryText = do
    t <- askInventory
    i <- askItem
    o <- askOwned
    s <- get
    let header = T.unlines ["---",t,T.center nameWidth ' ' i <> T.center numberWidth ' ' o]
    return $ T.unlines
           $ (header:)
           $ map f
           $ sortOn (runIdentity . iName . unItemI . fst)
           $ HM.toList
           $ inventory s
    where
        f (i,q) = indent $ itemNameText (runIdentity $ iName $ unItemI i) <> quantityText q

