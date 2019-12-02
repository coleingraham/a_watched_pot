{-# LANGUAGE OverloadedStrings #-}

module Game.Render.Scene      where

import           Control.Monad.IO.Class
import           Control.Monad.State.Strict hiding (State)
import           Data.Functor.Identity
import qualified Data.HashMap.Strict as HM
import           Data.IORef
import           Data.List
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text           as T
import qualified Data.Text.IO        as TIO
import           System.Console.Haskeline

import           Game.Item
import           Game.Language
import           Game.Monad
import           Game.Render.Text
import           Game.Scene
import qualified Game.State as GS
import           Game.Types

getInputText :: T.Text -> Game (Maybe T.Text)
getInputText prompt = do
    input <- lift $ lift $ getInputLine $ T.unpack prompt
    return $ fmap (T.strip . T.pack) input

updateComplete :: [String] -> Game ()
updateComplete ss = do
    ref <- askCompleteList
    liftIO $ writeIORef ref ss

showInventory :: Game ()
showInventory = inventoryText >>= liftIO . TIO.putStrLn

showLocation :: T.Text -> Game ()
showLocation loc = do
    liftIO $ TIO.putStrLn ""
    liftIO $ TIO.putStrLn $ "*** " <> loc <> " ***"

-- |Localizes and applies global actions.
getActionMap :: Scene Identity -> Game (ActionMap Identity)
getActionMap s = do
    global <- askActionMap
    case s of
        (Normal _ x ) -> return $ global <> nsActions x
        (Shop   _ x ) -> return $ global <> shActions x
        Inventory     -> return mempty -- at least for now
        Quit          -> return mempty

setCurrentScene :: SceneName -> Game ()
setCurrentScene sn = do
    s <- get
    put $ s { GS.currentScene = sn }

handleScene :: SceneName -> Game ()
handleScene sn = do
    sm <- askSceneMap
    let s = sm HM.! sn
    am <- getActionMap s
    case s of
        (Normal p s) -> setCurrentScene sn >> handleNormal am p s
        (Shop   p s) -> setCurrentScene sn >> handleShop   am p s
        Inventory    -> do
            current <- gets GS.currentScene
            showInventory
            handleScene current
        Quit         -> askGoodbuy >>= liftIO . TIO.putStrLn

handleNormal :: ActionMap Identity -> Prompt -> NormalScene Identity -> Game ()
handleNormal am (Prompt p) scene = showLocation p >> loop
    where
        acts = sort $ map (T.unpack . unAction . runIdentity) $ HM.keys am

        loop = do
            sm   <- askSceneMap
            updateComplete acts
            liftIO $ TIO.putStrLn $ unSceneText $ runIdentity $ nsText scene
            input <- getInputText $ p <> ": "
            handleInput input

        handleInput Nothing = loop
        handleInput (Just i)
            | isJust act = handleScene (fromJust act)
            | otherwise  = do
                liftIO $ TIO.putStrLn $ unSceneText $ runIdentity $ nsUnkown scene
                loop
            where
                act = HM.lookup (Identity $ Action i) am

handleShop :: ActionMap Identity -> Prompt -> ShopScene Identity -> Game ()
handleShop am (Prompt p) scene = showLocation p >> loop
    where
        items = shItems scene
        acts  = sort $ map (T.unpack . unAction . runIdentity) $ HM.keys am
        -- TODO: localize items!
        shop = sort $ map (T.unpack . unItemName . runIdentity) $ HM.keys items

        loop = do
            sm   <- askSceneMap
            updateComplete $ acts <> shop
            liftIO $ TIO.putStrLn $ unSceneText $ runIdentity $ shText scene
            shopText (HM.elems items) >>= liftIO . TIO.putStrLn
            w <- gets GS.wallet
            money <- askMoney
            liftIO $ TIO.putStrLn $ money <> ": " <> T.pack (show $ unMoney w)
            input <- getInputText $ p <> ": "
            handleInput sm input

        handleInput _  Nothing  = loop
        handleInput sm (Just i)
            | isJust act = handleScene (fromJust act)

            | isJust buy = do
                st <- get
                case GS.attemptToBuy (fromJust buy) st of
                    Nothing -> do
                        liftIO $ TIO.putStrLn $ unSceneText $ runIdentity $ shFailed scene
                        loop
                    Just st' -> do
                        liftIO $ TIO.putStrLn ""
                        liftIO $ TIO.putStrLn $ unSceneText $ runIdentity $ shThanks scene
                        put st'
                        loop

            | otherwise  = do
                liftIO $ TIO.putStrLn $ unSceneText $ runIdentity $ shUnkown scene
                loop
            where
                act = HM.lookup (Identity $ Action i) am
                buy = HM.lookup (Identity $ ItemName i) items

