

module Game.Monad (
 Config
,Global(..)
,Game
,runGame
,askCompleteList
,askSceneMap
,askActionMap
,askGoodbuy
,askMoney
,askItem
,askPrice
,askOwned
,askInventory
) where

import           Control.Arrow              (first)
import           Control.Monad.Reader
import           Control.Monad.State.Strict hiding (State)
import           Data.Aeson
import           Data.Functor.Identity
import qualified Data.HashMap.Strict        as HM
import           Data.IORef
import           Data.List
import qualified Data.Text                  as T
import           System.Console.Haskeline

import           Game.Language
import           Game.Scene
import qualified Game.State as GS
import           Game.Types

data Global f = Global {
         gScenes    :: !(SceneMap f)
        ,gActions   :: !(ActionMap f)
        ,gGoodbye   :: !(f T.Text)
        ,gMoney     :: !(f T.Text)
        ,gInventory :: !(f T.Text)
        ,gPrice     :: !(f T.Text)
        ,gItem      :: !(f T.Text)
        ,gOwned     :: !(f T.Text)
    }

instance Localizable Global where
    runLocalize l (Global sm am gb m inv p it o)
        = Global
            (localizeSceneMap  l sm)
            (localizeActionMap l am)
            (handleLocalize    l gb)
            (handleLocalize    l m)
            (handleLocalize    l inv)
            (handleLocalize    l p)
            (handleLocalize    l it)
            (handleLocalize    l o)

newtype GlobalL = GlobalL (Global Internationalized)

instance ToJSON GlobalL where
    toJSON = undefined

instance FromJSON GlobalL where
    parseJSON = undefined

data Config = Config {
         cRef     :: !(IORef [String])
         ,cGlobal :: !(Global Identity)
    }

type Game = ReaderT Config (StateT GS.State (InputT IO))

runGame
    :: IORef [String]
    -> Global Identity
    -> GS.State
    -> Game a
    -> IO a
runGame ref g s f = runInputT (settings ref) $ evalStateT (runReaderT f conf) s
    where
        conf = Config ref g

        compleations :: IORef [String] -> String -> IO [Completion]
        compleations ref s = do
            ns <- readIORef ref
            return $ map simpleCompletion $ filter (s `isPrefixOf`) ns

        settings :: IORef [String] -> Settings IO
        settings ref =
            setComplete
            (completeWord
                Nothing
                " \t"
                (compleations ref))
            defaultSettings


askCompleteList :: Game (IORef [String])
askCompleteList = asks cRef

askSceneMap :: Game (SceneMap Identity)
askSceneMap = asks (gScenes . cGlobal)

askActionMap :: Game (ActionMap Identity)
askActionMap = asks (gActions . cGlobal)

askGoodbuy :: Game T.Text
askGoodbuy = asks (runIdentity . gGoodbye . cGlobal)

askMoney :: Game T.Text
askMoney = asks (runIdentity . gMoney . cGlobal)

askItem :: Game T.Text
askItem = asks (runIdentity . gItem . cGlobal)

askPrice :: Game T.Text
askPrice = asks (runIdentity . gPrice . cGlobal)

askOwned :: Game T.Text
askOwned = asks (runIdentity . gOwned . cGlobal)

askInventory :: Game T.Text
askInventory = asks (runIdentity . gInventory . cGlobal)

