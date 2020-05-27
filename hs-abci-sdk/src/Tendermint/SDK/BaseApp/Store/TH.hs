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
  :: Name
  -- ^ Namespace
  -> Name
  -- ^ Type
  -> String 
  -- ^ key
  -> Q [Dec]
makeVarType namespaceName typeName key = do
  let dataDecl = DataD [] keyTypeName [] Nothing [NormalC keyTypeName []] []
  rawKeyInst <- instanceD (pure []) (conT ''RawKey `appT` conT keyTypeName) [funD 'rawKey [genRawKeyClause]]
  isKeyInst <- instanceD (pure []) (conT ''IsKey `appT` conT keyTypeName `appT` conT namespaceName) 
    [tySynInstD ''Value $ tySynEqn [conT keyTypeName, conT namespaceName] (conT ''Var `appT` conT typeName)]
  pure [dataDecl, rawKeyInst, isKeyInst]
  where
      keyTypeName = mkName $ nameBase typeName ++ "Key"
      keyBytes = cs @ByteString @String . convert . hashWith SHA256 . cs @String @ByteString $ key
      genRawKeyClause = do
          body <- [| iso (const $ cs @String @ByteString $(litE $ StringL keyBytes)) (const $(conE keyTypeName)) |]
          pure $ Clause [] (NormalB body) []
