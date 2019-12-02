{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Game.State where

import           Codec.Compression.GZip
import           Control.Arrow
import           Data.Binary
import qualified Data.ByteString.Lazy as BS
import           Data.Functor.Identity
import           Data.Hashable
import qualified Data.HashMap.Strict as HM
import           Data.Monoid
import           Data.Time.Clock
import           Data.Time.Format

import qualified Game.Item as I
import           Game.Language
import           Game.Types

newtype Quantity = Quantity {
        unQuantity :: Int
    } deriving (Show,Eq,Num,Binary,Hashable)

data State = State {
         lastSessionTime :: !UTCTime
        ,inventory       :: !(HM.HashMap I.ItemI (Sum Quantity))
        ,wallet          :: !I.Money
        ,currentScene    :: !SceneName
        ,currentLanguage :: !Language -- ^used for updating 'inventory' if the language changes
    }

tFmt :: String
tFmt = "%d-%m-%Y %H:%M:%S"

encodeTime :: UTCTime -> String
encodeTime = formatTime defaultTimeLocale tFmt

decodeTime :: String -> UTCTime
decodeTime = parseTimeOrError True defaultTimeLocale tFmt

instance Binary State where
    put s = do
        put $ encodeTime $ lastSessionTime s
        put $ HM.toList $ HM.map getSum $ inventory s
        put $ wallet s
        put $ currentScene s
        put $ currentLanguage s

    get = State
      <$> (decodeTime <$> get)
      <*> (HM.map Sum . HM.fromList <$> get)
      <*> get
      <*> get
      <*> get

instance Hashable State where
    hashWithSalt s state = undefined

newState :: SceneName -> Language -> IO State
newState scene lang = State
       <$> getCurrentTime
       <*> mempty
       <*> pure 100
       <*> pure scene
       <*> pure lang

--writeState :: FilePath -> State -> IO ()
--writeState fp = BS.writeFile fp . compress . encode
--
--readState :: FilePath -> IO State
--readState fp = decode . decompress <$> BS.readFile fp
--
attemptToBuy :: I.Item Identity -> State -> Maybe State
attemptToBuy i s = do
    m <- I.attemptToBuyItem i $ wallet s
    return $ s { inventory = HM.insertWith (<>) (I.ItemI i) 1 (inventory s)
               , wallet    = m }

