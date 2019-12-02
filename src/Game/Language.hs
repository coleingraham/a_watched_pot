{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Game.Language where

import           Data.Aeson
import           Data.Binary          (Binary)
import           Data.Functor.Identity
import           Data.Hashable        (Hashable)
import qualified Data.HashMap.Strict  as HM
import           Data.Maybe
import           Data.String          (IsString)
import qualified Data.Text            as T

-- |Represents a specific language.
newtype Language = Language {
        unLanguage :: T.Text
    } deriving (Show,Eq,IsString,Binary,Hashable,ToJSON,ToJSONKey,FromJSON,FromJSONKey)

-- |US English.
englishUS :: Language
englishUS = "en-us"

-- |Holds all translations of something.
type Internationalized = HM.HashMap Language

localize :: Language -> Internationalized a -> a
localize l hm = fromMaybe (hm HM.! englishUS) (HM.lookup l hm)

class Localizable a where
    runLocalize :: Language -> a Internationalized -> a Identity

handleLocalize :: Language -> Internationalized a -> Identity a
handleLocalize l = Identity . localize l

