
module Network.ABCI.Types.MessagesSpec where

-- NOTE can't import this, getting:
--      Could not load module ‘Proto.Types’
--      it is a hidden module in the package ‘hs-abci-server-0.1.0.0’
--      Use -v to see a list of the files searched for.
-- import qualified Proto.Types as PT
import           Control.Lens                           (Iso', from, (^.))
import           Data.ProtoLens.Message                 (Message)
import           Network.ABCI.Types.Messages.Instances
import           Network.ABCI.Types.Messages.FieldTypes as FieldTypes
import           Network.ABCI.Types.Messages.Request    as Request
import           Network.ABCI.Types.Messages.Response   as Response
import           Test.Hspec
import           Data.ProtoLens.Arbitrary              (ArbitraryMessage(..))
import           Test.QuickCheck


main :: IO ()
main = hspec spec

isoCheck :: (Eq b, Eq a, Show b, Show a, Arbitrary a, Message b) => String -> Iso' a b -> SpecWith ()
isoCheck name iso = do
  describe name $ do
    -- NOTE we might need to clear "unknownFields" from arbitrary
    -- messages as we don't store them in wrapped types and it would brake the test
    it "P > W > P == P" $ property $ check iso . unArbitraryMessage
    it "W > P > W == W" $ property $ check (from iso)
  where
    check :: forall a' b'. (Eq b') => Iso' a' b' -> b' -> Bool
    check iso' val =
      let 
        -- a :: a'
        a = val ^. isoFlipped
        -- isoFlipped :: Iso' b' a'
        isoFlipped = from iso'
        -- b :: b'
        b = a ^. iso'
      in val == b

spec :: Spec
spec = do
  describe "Request" $ do
    isoCheck "echo" Request.echo
    isoCheck "flush" Request.flush
    isoCheck "info" Request.info
    isoCheck "setOption" Request.setOption
    isoCheck "initChain" Request.initChain
    isoCheck "query" Request.query
    isoCheck "beginBlock" Request.beginBlock
    isoCheck "checkTx" Request.checkTx
    isoCheck "deliverTx" Request.deliverTx
    isoCheck "endBlock" Request.endBlock
    isoCheck "commit" Request.commit
  describe "Response" $ do
      isoCheck "echo" Response.echo
      isoCheck "flush" Response.flush
      isoCheck "info" Response.info
      isoCheck "setOption" Response.setOption
      isoCheck "initChain" Response.initChain
      isoCheck "query" Response.query
      isoCheck "beginBlock" Response.beginBlock
      isoCheck "checkTx" Response.checkTx
      isoCheck "deliverTx" Response.deliverTx
      isoCheck "endBlock" Response.endBlock
      isoCheck "commit" Response.commit
      isoCheck "exception" Response.exception
  -- describe "FieldTypes" $ do
    -- timestamp :: Iso' Timestamp T.Timestamp
    -- blockSizeParams :: Iso' BlockSizeParams PT.BlockSizeParams
    -- evidenceParams :: Iso' EvidenceParams PT.EvidenceParams
    -- validatorParams :: Iso' ValidatorParams PT.ValidatorParams
    -- consensusParams :: Iso' ConsensusParams PT.ConsensusParams
    -- pubKey :: Iso' PubKey PT.PubKey
    -- validatorUpdate :: Iso' ValidatorUpdate PT.ValidatorUpdate
    -- validator :: Iso' Validator PT.Validator
    -- voteInfo :: Iso' VoteInfo PT.VoteInfo
    -- lastCommitInfo :: Iso' LastCommitInfo PT.LastCommitInfo
    -- partSetHeader :: Iso' PartSetHeader PT.PartSetHeader
    -- blockID :: Iso' BlockID PT.BlockID
    -- version :: Iso' Version PT.Version
    -- header :: Iso' Header PT.Header
    -- evidence :: Iso' Evidence PT.Evidence
    -- kVPair :: Iso' KVPair CT.KVPair
    -- proof :: Iso' Proof MT.Proof
    -- proofOp :: Iso' ProofOp MT.ProofOp
  --   pure unit
--     it "works for an empty bytestring" $
--       beWordFromBytes BS.empty `shouldBe` Right 0

--     it "works for 1 byte length bytestrings" $ property $ \word ->
--       beWordFromBytes (runPut (Put.putWord8 word)) == Right (fromIntegral word)

--     it "works for 2 byte length bytestrings" $ property $ \word ->
--       beWordFromBytes (runPut (Put.putWord16be word)) == Right (fromIntegral word)

--     it "works for 4 byte length bytestrings" $ property $ \word ->
--       beWordFromBytes (runPut (Put.putWord32be word)) == Right (fromIntegral word)

--     it "works for 8 byte length bytestrings" $ property $ \word ->
--       beWordFromBytes (runPut (Put.putWord64be word)) == Right word

--     it "fails for bytestring larger than 8 bytes" $
--       let arbitraryBsLenGt8 =
--             choose (9,2000) >>= randomBytestringOfLength
--       in forAll arbitraryBsLenGt8 $ \bs ->
--         beWordFromBytes bs `shouldSatisfy` isLeft

--   describe "decodeLengthPrefixC / encodeLengthPrefixC" $ do

--     -- | This property test (see https://en.wikipedia.org/wiki/QuickCheck)
--     -- for pointers on what this is) checks that these functions are the
--     -- inverse of each other for arbitrarily chunked arbitrary 'ByteString's.
--     -- When the 'Producer' is a real socket, each chunk is the result of a
--     -- 'recv' call. We check for robustness against a peer that decides to
--     -- flush their side of the connection in funky ways.

--     it "decoding an encoded bytestring yields the same bytestring" $
--       property $ \(bytelist, nonNegativeChunkSizes) ->
--         let conduit = chunksProducer bytes nonNegativeChunkSizes
--                    .| CL.map (\a -> [a])
--                    .| encodeLengthPrefixC
--                    .| decodeLengthPrefixC
--                    .| consumeValidChunks
--             bytes = BS.pack bytelist
--         in runIdConduit conduit == Right bytes

--   describe "decodeLengthPrefixC" $ do

--     it "fails gracefully when given a string larger than maxMessageLen" $
--       let ginormousSizeVarLen = 8 `BS.cons` runPut (Put.putWord64be maxBound)
--           conduit = yield ginormousSizeVarLen
--                  .| decodeLengthPrefixC
--                  .| consumeValidChunks
--       in runIdConduit conduit `shouldSatisfy` isLeft


-- -- Takes a 'ByteString' and a list of chunkSizes >= 0 and yields the
-- -- 'ByteString' split into chunks of the given sizes
-- --
-- -- We allow empty chunks to check how they're handled. However, these
-- -- are probably never produced when the real 'Producer' is a socket
-- -- since the underlying 'recv' call should, in principle, never yield
-- -- an empty chunk as this means our peer has closed their side of
-- -- the connection (according to https://hackage.haskell.org/package/network-bytestring-0.1.3.4/docs/Network-Socket-ByteString.html#v:recv)
-- chunksProducer
--   :: Monad m
--   => BS.ByteString -> [NonNegative Int] -> ConduitT () BS.ByteString m ()
-- chunksProducer bs [] = yield bs
-- chunksProducer bs _ | BS.null bs = return ()
-- chunksProducer bs (c:cs) = yield chunk >> chunksProducer rest cs
--   where (chunk, rest) = BS.splitAt (getNonNegative c) bs

-- randomBytestringOfLength :: Int -> Gen BS.ByteString
-- randomBytestringOfLength len = BS.pack <$> replicateM len arbitrary

-- runIdConduit :: ConduitT () Void Identity a -> a
-- runIdConduit = runIdentity . runConduit

-- -- This consumer will concatenate all the valid decoded chunks.
-- -- The first error encountered will be the result of the 'Consumer'
-- -- without checking for further errors
-- consumeValidChunks
--   :: Monad m
--   => ConduitT (Either String [BS.ByteString]) Void m (Either String BS.ByteString)
-- consumeValidChunks = CL.fold step (Right BS.empty)
--   where
--     step (Right acc) (Right [s])  = Right (acc <> s)
--     step (Right _) (Right _)  = Left "Expecting Singleton List"
--     step (Right _)   (Left err) = Left err
--     step (Left err)  _          = Left err

-- runPut :: Put.Put -> BS.ByteString
-- runPut = LBS.toStrict . Put.runPut