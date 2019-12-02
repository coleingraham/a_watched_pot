

module Game.Dialog where

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

data Dialog = Dialog {
         dText :: !T.Text
        ,dCont :: !Continuation
    } deriving (Show,Eq)

data Continuation
    = End
    | More   !Dialog
    | Choice !(HM.HashMap T.Text Dialog)
    deriving (Show,Eq)

