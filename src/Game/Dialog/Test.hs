{-# LANGUAGE OverloadedStrings #-}

module Game.Dialog.Test where

import qualified Data.HashMap.Strict as HM

import           Game.Dialog

single :: Dialog
single = Dialog "Hello world!" End

choice :: Dialog
choice
    = Dialog "What is the answer?" $ Choice $ HM.fromList
        [("yes",Dialog "Cool" End)
        ,("no" ,Dialog "Boo" $ Choice $ HM.fromList
            [("yes",Dialog "Cool" End)
            ,("no" ,Dialog "Boooooo" End)])]

