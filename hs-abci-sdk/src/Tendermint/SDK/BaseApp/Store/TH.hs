{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module Tendermint.SDK.BaseApp.Store.TH
  ( makeVarType
  ) where


import           Control.Lens                          (iso)
import           Crypto.Hash                           (SHA256 (..), hashWith)
import           Data.ByteArray                        (convert)
import           Data.ByteString                       (ByteString)
import           Data.String.Conversions               (cs)
import           Language.Haskell.TH
import           Tendermint.SDK.BaseApp.Store.RawStore (RawKey (..), IsKey(..))
import           Tendermint.SDK.BaseApp.Store.Var as V


makeVarType 
  :: String
  -- ^ Namespace
  -> String 
  -- ^ Type
  -> String 
  -- ^ key
  -> Q [Dec]
makeVarType namespace _type key = do
  let dataDecl = DataD [] keyTypeName [] Nothing [NormalC keyTypeName []] []
  rawKeyInst <- instanceD (pure []) (conT ''RawKey `appT` conT keyTypeName) [funD 'rawKey [genRawKeyClause]]
  isKeyInst <- instanceD (pure []) (conT ''IsKey `appT` conT keyTypeName) 
    [tySynD ''Value [PlainTV keyTypeName, PlainTV namespaceName] (conT ''Var `appT` conT typeName)]
  pure [dataDecl, rawKeyInst, isKeyInst]
  where
      typeName = mkName _type
      namespaceName = mkName namespace
      keyTypeName = mkName $ _type ++ "Key"
      keyBytes = cs @ByteString @String . convert . hashWith SHA256 . cs @String @ByteString $ key
      genRawKeyClause = do
          body <- [| iso (const $ cs @String @ByteString $(litE $ StringL keyBytes)) (const $(conE keyTypeName)) |]
          pure $ Clause [] (NormalB body) []
