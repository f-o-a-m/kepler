{-# LANGUAGE RankNTypes #-}
module Network.ABCI.Test.Internal.WireSpec (main, spec) where

import           Network.ABCI.Internal.Wire
import qualified Network.ABCI.Types.Error as Error
import qualified Data.Binary.Put as Put
import qualified Data.ByteString as BS
import           Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as LBS
import           Data.Conduit
import qualified Data.Conduit.List as CL
import           Data.Either (isLeft)
import           Data.Functor.Identity (Identity(runIdentity))
import           Data.Monoid ((<>))
import           Test.Hspec
import           Test.QuickCheck


main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "decodeLengthPrefixC / encodeLengthPrefixC" $ do

    -- | This property test (see https://en.wikipedia.org/wiki/QuickCheck)
    -- for pointers on what this is) checks that these functions are the
    -- inverse of each other for arbitrarily chunked arbitrary 'ByteString's.
    -- When the 'Producer' is a real socket, each chunk is the result of a
    -- 'recv' call. We check for robustness against a peer that decides to
    -- flush their side of the connection in funky ways.

    it "decoding an encoded bytestring yields the same bytestring" $
      property $ \(bytelist, nonNegativeChunkSizes) ->
        let conduit = chunksProducer bytes nonNegativeChunkSizes
                   .| CL.map (\a -> [a])
                   .| encodeLengthPrefixC
                   .| decodeLengthPrefixC
                   .| CL.map (first Error.print)
                   .| consumeValidChunks
            bytes = BS.pack bytelist
        in runIdConduit conduit == Right bytes

  describe "decodeLengthPrefixC" $ do

    it "fails gracefully when given a string larger than maxMessageLen" $
      let ginormousSizeVarLen = 8 `BS.cons` runPut (Put.putWord64be maxBound)
          conduit = yield ginormousSizeVarLen
                 .| decodeLengthPrefixC
                 .| CL.map (first Error.print)
                 .| consumeValidChunks
      in runIdConduit conduit `shouldSatisfy` isLeft


-- Takes a 'ByteString' and a list of chunkSizes >= 0 and yields the
-- 'ByteString' split into chunks of the given sizes
--
-- We allow empty chunks to check how they're handled. However, these
-- are probably never produced when the real 'Producer' is a socket
-- since the underlying 'recv' call should, in principle, never yield
-- an empty chunk as this means our peer has closed their side of
-- the connection (according to https://hackage.haskell.org/package/network-bytestring-0.1.3.4/docs/Network-Socket-ByteString.html#v:recv)
chunksProducer
  :: Monad m
  => BS.ByteString -> [NonNegative Int] -> ConduitT () BS.ByteString m ()
chunksProducer bs [] = yield bs
chunksProducer bs _ | BS.null bs = return ()
chunksProducer bs (c:cs) = yield chunk >> chunksProducer rest cs
  where (chunk, rest) = BS.splitAt (getNonNegative c) bs

runIdConduit :: ConduitT () Void Identity a -> a
runIdConduit = runIdentity . runConduit

-- This consumer will concatenate all the valid decoded chunks.
-- The first error encountered will be the result of the 'Consumer'
-- without checking for further errors
consumeValidChunks
  :: Monad m
  => ConduitT (Either String [BS.ByteString]) Void m (Either String BS.ByteString)
consumeValidChunks = CL.fold step (Right BS.empty)
  where
    step (Right acc) (Right [s]) = Right (acc <> s)
    step (Right _) (Right _) = Left "Expecting Singleton List"
    step (Right _) (Left err) = Left err
    step (Left err) _ = Left err

runPut :: Put.Put -> BS.ByteString
runPut = LBS.toStrict . Put.runPut
