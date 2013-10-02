{-# OPTIONS_GHC -fno-warn-orphans #-}

import Control.Applicative
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import Data.Monoid
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Network.HTTP.QueryString.Internal

instance Arbitrary ByteString where
    arbitrary = BC.pack <$> arbitrary

data KeyString = Key { toBS :: ByteString }

instance Arbitrary KeyString where
    arbitrary = Key . BC.pack <$> ((:) <$> arbitrary <*> arbitrary)

instance Arbitrary QueryString where
    arbitrary = queryString . map unpack <$> arbitrary

unpack :: (KeyString, ByteString) -> (ByteString, ByteString)
unpack (a, b) = (toBS a, b)

instance Show QueryString where
    show = show . rawData

main :: IO ()
main = hspec $ do
    describe "QueryString" $ do
        prop "mappend" prop_mappend
        prop "read" prop_read
        prop "parse" prop_parse

prop_mappend :: QueryString -> QueryString -> Bool
prop_mappend a b = (a <> b) == queryString (rawData a ++ rawData b)

prop_read :: QueryString -> Bool
prop_read a = read (BC.unpack (toString a)) == a

prop_parse :: QueryString -> Bool
prop_parse a = parseQuery (toString a) == Just a
