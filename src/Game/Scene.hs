{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Game.Scene where

import           Control.Applicative
import           Control.Arrow        (first)
import           Data.Aeson
import           Data.Binary          (Binary)
import qualified Data.ByteString.Lazy as BS
import           Data.Functor.Identity
import           Data.Hashable
import qualified Data.HashMap.Strict  as HM
import           Data.Maybe
import           Data.String          (IsString)
import qualified Data.Text            as T
import qualified Data.Vector          as V

import           Game.Item
import           Game.Language
import           Game.Types

type ActionMap f = HM.HashMap (f Action) SceneName

-- |Only used for making to/from JSON instances.
newtype ActionMapL = ActionMapL {
        unActionMapL :: ActionMap Internationalized
    }

instance ToJSON ActionMapL where
    toJSON (ActionMapL hm) = toJSON $ map f $ HM.toList hm
        where
            f (ks,v) = object [ "action" .= ks
                              , "scene"  .= v ]

instance FromJSON ActionMapL where
    parseJSON = withArray "action_map" $ \v ->
        ActionMapL . HM.fromList <$> mapM f (V.toList v)
        where
            f (Object x) = do
                ks <- x .: "action"
                v  <- x .: "scene"
                return (ks,v)

localizeActionMap :: Language -> ActionMap Internationalized -> ActionMap Identity
localizeActionMap l = HM.fromList . map (first (handleLocalize l)) . HM.toList

getActions :: ActionMap Identity -> [Action]
getActions = map runIdentity . HM.keys

runAction :: Action -> ActionMap Identity -> SceneName
runAction a hm = hm HM.! Identity a

data Scene f
    = Normal !Prompt !(NormalScene f)
    | Shop   !Prompt !(ShopScene f)
    | Inventory
    | Quit

instance Localizable Scene where
    runLocalize l (Normal p s) = Normal p $ runLocalize l s
    runLocalize l (Shop   p s) = Shop   p $ runLocalize l s
    runLocalize _ Inventory    = Inventory
    runLocalize _ Quit         = Quit

newtype SceneL = SceneL {
        unSceneL :: Scene Internationalized
    }

--instance Hashable Scene

instance ToJSON SceneL where
    toJSON (SceneL (Normal p x)) = object [ "prompt" .= p, "scene" .= NormalSceneL x ]
    toJSON (SceneL (Shop   p x)) = object [ "prompt" .= p, "shop"  .= ShopSceneL x ]
    toJSON (SceneL Inventory)    = "inventory"
    toJSON (SceneL Quit)         = "quit"

instance FromJSON SceneL where
    parseJSON x
        = (withObject "scene" $ \o -> do
            p                <- o .: "prompt"
            (NormalSceneL s) <- o .: "scene"
            return $ SceneL $ Normal p s) x

      <|> (withObject "scene" $ \o -> do
            p              <- o .: "prompt"
            (ShopSceneL s) <- o .: "shop"
            return $ SceneL $ Shop p s) x

      <|> (withText "scene" $ \case
            "inventory" -> return $ SceneL Inventory
            "quit"      -> return $ SceneL Quit
            ) x

-- |A 'Scene' where a user can read a description and perform specific actions.
data NormalScene f = NormalScene {
         nsText    :: !(f SceneText) -- ^The main description of the scene
        ,nsUnkown  :: !(f SceneText)
        ,nsActions :: !(ActionMap f) -- ^All scene-specific actions
    }

instance Localizable NormalScene where
    runLocalize l (NormalScene t u as)
        = NormalScene
            (handleLocalize l t)
            (handleLocalize l u)
            (localizeActionMap l as)

newtype NormalSceneL = NormalSceneL {
        unNormalSceneL :: NormalScene Internationalized
    }

--instance Hashable NormalScene where
--    hashWithSalt s (NormalScene t u as) = undefined

instance ToJSON NormalSceneL where
    toJSON (NormalSceneL x) = object
        [ "text"    .= nsText    x
        , "unknown" .= nsUnkown  x
        , "actions" .= ActionMapL (nsActions x) ]

instance FromJSON NormalSceneL where
    parseJSON = withObject "normal_scene" $ \o ->
        NormalSceneL <$> (NormalScene <$> o .: "text"
                                      <*> o .: "unknown"
                                      <*> (unActionMapL <$> o .: "actions"))

data ShopScene f = ShopScene {
         shText    :: !(f SceneText)
        ,shItems   :: !(ItemMap f)
        ,shFailed  :: !(f SceneText)
        ,shThanks  :: !(f SceneText)
        ,shUnkown  :: !(f SceneText)
        ,shActions :: !(ActionMap f)
    }

instance Localizable ShopScene where
    runLocalize l (ShopScene t i f th u as)
        = ShopScene
            (handleLocalize l t)
            (localizeItemMap l i)
            (handleLocalize l f)
            (handleLocalize l th)
            (handleLocalize l u)
            (localizeActionMap l as)

newtype ShopSceneL = ShopSceneL {
        unShopSceneL :: ShopScene Internationalized
    }

instance ToJSON ShopSceneL where
    toJSON (ShopSceneL x) = object
        [ "text"    .= shText     x
        , "items"   .= ItemMapL   (shItems   x)
        , "failed"  .= shFailed   x
        , "thanks"  .= shThanks   x
        , "unknown" .= shUnkown   x
        , "unknown" .= shUnkown   x
        , "actions" .= ActionMapL (shActions x) ]

instance FromJSON ShopSceneL where
    parseJSON = withObject "normal_scene" $ \o ->
        ShopSceneL <$> (ShopScene <$> o .: "text"
                                  <*> (unItemMapL <$> o .: "items")
                                  <*> o .: "failed"
                                  <*> o .: "thanks"
                                  <*> o .: "unknown"
                                  <*> (unActionMapL <$> o .: "actions"))

--instance Hashable ShopScene

type SceneMap f = HM.HashMap SceneName (Scene f)

newtype SceneMapL = SceneMapL {
        unSceneMapL :: SceneMap Internationalized
    }

instance ToJSON SceneMapL where
    toJSON (SceneMapL hm) = toJSON $ HM.map SceneL hm

instance FromJSON SceneMapL where
    parseJSON x = SceneMapL . HM.map unSceneL <$> parseJSON x

localizeSceneMap :: Language -> SceneMap Internationalized -> SceneMap Identity
localizeSceneMap l = HM.map (runLocalize l)

--readScene
--    :: FilePath -- ^must be relative
--    -> IO (Maybe SceneMap)
--readScene fp = fmap (HM.singleton sn) . decode <$> BS.readFile fp
--    where
--        sn = SceneName $ T.pack fp

