{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Game.Types where

import           Data.Aeson
import           Data.Binary          (Binary)
import           Data.Hashable        (Hashable)
import qualified Data.HashMap.Strict  as HM
import           Data.String          (IsString)
import qualified Data.Text            as T

-- |The name of a specific 'Scene'.
newtype SceneName = SceneName {
        unSceneName :: T.Text
    } deriving (Show,Eq,IsString,Binary,Hashable,ToJSON,ToJSONKey,FromJSON,FromJSONKey)

-- |The description of a 'Scene'.
newtype SceneText = SceneText {
        unSceneText :: T.Text
    } deriving (Show,Eq,IsString,Binary,Hashable,ToJSON,ToJSONKey,FromJSON,FromJSONKey)

-- |A command a user can type do interact with a 'Scene'.
newtype Action = Action {
        unAction :: T.Text
    } deriving (Show,Eq,IsString,Binary,Hashable,ToJSON,ToJSONKey,FromJSON,FromJSONKey)

newtype Prompt = Prompt {
        unPrompt :: T.Text
    } deriving (Show,Eq,IsString,Binary,Hashable,ToJSON,ToJSONKey,FromJSON,FromJSONKey)

