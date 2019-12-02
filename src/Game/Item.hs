{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Game.Item where

import           Control.Arrow
import           Data.Aeson
import           Data.Binary
import           Data.Functor.Identity
import           Data.Hashable
import qualified Data.HashMap.Strict  as HM
import           Data.String          (IsString)
import qualified Data.Text            as T
import qualified Data.Vector          as V

import           Game.Language

newtype Money = Money {
        unMoney :: Int
    } deriving (Show,Eq,Ord,Num,Binary,Hashable,ToJSON,ToJSONKey,FromJSON,FromJSONKey)

newtype Worth = Worth {
         unWorth :: Money
    } deriving (Show,Eq,Ord,Num,Binary,Hashable,ToJSON,ToJSONKey,FromJSON,FromJSONKey)

buyPrice :: Worth -> Money
buyPrice = unWorth

sellPrice :: Worth -> Money
sellPrice = Money . ceiling . (* 0.75) . fromIntegral . unMoney . unWorth

attemptToBuy :: Worth -> Money -> Maybe Money
attemptToBuy w m
    | p > m     = Nothing
    | otherwise = Just $ m - p
    where
        p = buyPrice w

sell :: Worth -> Money -> Money
sell w = (+) (sellPrice w)

newtype ItemName = ItemName {
        unItemName :: T.Text
    } deriving (Show,Eq,Ord,IsString,Binary,Hashable,ToJSON,ToJSONKey,FromJSON,FromJSONKey)

newtype ItemDescription = ItemDescription {
        unItemDescription :: T.Text
    } deriving (Show,Eq,IsString,Binary,Hashable,ToJSON,ToJSONKey,FromJSON,FromJSONKey)

data Item f = Item {
         iName        :: !(f ItemName)
        ,iDescription :: !(f ItemDescription)
        ,iWorth       :: !Worth
    }

instance Localizable Item where
    runLocalize l (Item n d w) = Item (handleLocalize l n) (handleLocalize l d) w

newtype ItemL = ItemL (Item Internationalized)

instance Binary ItemL where
    put (ItemL (Item n d w)) = do
        put $ HM.toList n
        put $ HM.toList d
        put w

    get = ItemL <$> (Item
                <$> (HM.fromList <$> get)
                <*> (HM.fromList <$> get)
                <*> get)

instance Hashable ItemL where
    hashWithSalt s (ItemL (Item n d w))
        = hashWithSalt s s
        + hashWithSalt s n
        + hashWithSalt s d
        + hashWithSalt s w

instance ToJSON ItemL where
    toJSON (ItemL x) = object
        [ "name"        .= iName        x
        , "description" .= iDescription x
        , "worth"       .= iWorth       x ]

instance FromJSON ItemL where
    parseJSON = withObject "item" $ \o ->
        ItemL <$> (Item <$> o .: "name"
                        <*> o .: "description"
                        <*> o .: "worth")

newtype ItemI = ItemI {
        unItemI :: Item Identity
    }

instance Eq ItemI where
    (ItemI (Item na da wa)) == (ItemI (Item nb db wb))
        = na == nb && da == db && wa == wb

instance Hashable ItemI where
    hashWithSalt s (ItemI (Item n d w))
        = hashWithSalt s s
        + hashWithSalt s n
        + hashWithSalt s d
        + hashWithSalt s w

instance ToJSON ItemI where
    toJSON (ItemI x) = object
        [ "name"        .= iName        x
        , "description" .= iDescription x
        , "worth"       .= iWorth       x ]

instance FromJSON ItemI where
    parseJSON = withObject "item" $ \o ->
        ItemI <$> (Item <$> o .: "name"
                        <*> o .: "description"
                        <*> o .: "worth")

instance Binary ItemI where
    put (ItemI (Item n d w)) = do
        put $ runIdentity n
        put $ runIdentity d
        put w

    get = ItemI <$> (Item
                <$> (Identity <$> get)
                <*> (Identity <$> get)
                <*> get)

type ItemMap f = HM.HashMap (f ItemName) (Item f)

newtype ItemMapL = ItemMapL {
        unItemMapL :: ItemMap Internationalized
    }

instance ToJSON ItemMapL where
    toJSON (ItemMapL hm) = toJSON $ map f $ HM.toList hm
        where
            f (ks,v) = object [ "name" .= ks
                              , "item" .= ItemL v ]

instance FromJSON ItemMapL where
    parseJSON = withArray "item_map" $ \v ->
        ItemMapL . HM.fromList <$> mapM f (V.toList v)
        where
            f (Object x) = do
                ks        <- x .: "name"
                (ItemL v) <- x .: "item"
                return (ks,v)

localizeItemMap :: Language -> ItemMap Internationalized -> ItemMap Identity
localizeItemMap l = HM.fromList . map (handleLocalize l *** runLocalize l) . HM.toList

attemptToBuyItem :: Item Identity -> Money -> Maybe Money
attemptToBuyItem i = attemptToBuy (iWorth i)

sellItem :: Item Identity -> Money -> Money
sellItem i = sell (iWorth i)

