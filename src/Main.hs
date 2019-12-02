{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Functor.Identity
import qualified Data.HashMap.Strict as HM
import           Data.IORef
import           Data.Monoid

import           Game.Dialog.Test()
import           Game.Item
import           Game.Item.Test
import           Game.Language
import           Game.Monad
import           Game.Render.Scene
import           Game.Render.Text()
import           Game.Scene
import           Game.State
import           Game.Time()

main :: IO ()
main = do
    s   <- newState "town" lang
    ref <- newIORef []
    let g = runLocalize lang testGlobal
    runGame ref g s $ handleScene $ currentScene s
    where
        lang = englishUS

---- This will all come from files soon

townScene' :: SceneMap Internationalized
townScene' = HM.singleton "town" $ Normal "Town"
            $ NormalScene
                (HM.singleton englishUS "Do you want to [shop]")
                (HM.singleton englishUS "I don't understand")
                (HM.fromList
                    [(HM.singleton englishUS "shop","shop")])

shopScene' :: SceneMap Internationalized
shopScene' = HM.singleton "shop" $ Shop "Shop"
            $ ShopScene
                (HM.singleton englishUS "Yes?")
                (HM.fromList $ map (\x -> (iName x,x)) [beer,pizza])
                (HM.singleton englishUS "You don't have enough money for that!")
                (HM.singleton englishUS "Thank you for your business")
                (HM.singleton englishUS "Come again?")
                (HM.fromList
                    [(HM.singleton englishUS "leave","town")])

inventoryScene :: SceneMap Internationalized
inventoryScene = HM.singleton "inventory" Inventory

quitScene :: SceneMap Internationalized
quitScene = HM.singleton "quit" Quit

testSceneMap :: SceneMap Internationalized
testSceneMap = townScene' <> shopScene'

testGlobal :: Global Internationalized
testGlobal = Global
                (quitScene <> inventoryScene <> testSceneMap)
                (HM.fromList [(HM.singleton englishUS "inventory","inventory")
                             ,(HM.singleton englishUS "quit","quit")])
                (HM.singleton englishUS "Buy for now")
                (HM.singleton englishUS "Money")
                (HM.singleton englishUS "Inventory")
                (HM.singleton englishUS "Price")
                (HM.singleton englishUS "Item")
                (HM.singleton englishUS "Owned")

