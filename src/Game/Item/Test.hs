{-# LANGUAGE OverloadedStrings #-}

module Game.Item.Test where

import           Data.Functor.Identity
import qualified Data.HashMap.Strict as HM

import           Game.Item
import           Game.Language

cola :: Item Internationalized
cola = Item
    (HM.singleton englishUS "Cola")
    (HM.singleton englishUS "Not the best for you, but tastes alright")
    3

beer :: Item Internationalized
beer = Item
    (HM.singleton englishUS "Beer")
    (HM.singleton englishUS "Now we've talking!")
    5

pizza :: Item Internationalized
pizza = Item
    (HM.singleton englishUS "Pizza")
    (HM.singleton englishUS "It's a pizza")
    10

