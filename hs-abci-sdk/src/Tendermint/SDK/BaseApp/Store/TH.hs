{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module Tendermint.SDK.BaseApp.Store.TH
  ( makeSubStore
  , module Tendermint.SDK.BaseApp.Store.RawStore
  , module Tendermint.SDK.BaseApp.Store.Var
  , module Tendermint.SDK.BaseApp.Store.Array
  , module Tendermint.SDK.BaseApp.Store.List
  , module Tendermint.SDK.BaseApp.Store.Map
  ) where


import           Control.Lens                          (iso)
import           Crypto.Hash                           (SHA256 (..), hashWith)
import           Data.ByteArray                        (convert)
import           Data.ByteString                       (ByteString)
import           Data.String.Conversions               (cs)
import           Language.Haskell.TH
import           Tendermint.SDK.BaseApp.Store.Array    (Array, makeArray)
import           Tendermint.SDK.BaseApp.Store.List     (List, makeList)
import           Tendermint.SDK.BaseApp.Store.Map      (Map, makeMap)
import           Tendermint.SDK.BaseApp.Store.RawStore (IsKey (..), RawKey (..))
import           Tendermint.SDK.BaseApp.Store.Var      (Var, makeVar)


makeSubStore
  :: Name
  -- ^ store
  -> String
  -- ^ substoreName
  -> TypeQ
  -- ^ store type
  -> String
  -- ^ key
  -> Q [Dec]
makeSubStore storeName substoreName t key = do
  namespaceName <- getStoreNamespace storeName
  keyType <- parseKeyType <$> t
  let keyTypeName = mkKeyName keyType
      keyBytes = cs @ByteString @String . convert . hashWith SHA256 . cs @String @ByteString $ key
      dataDecl = DataD [] keyTypeName [] Nothing [NormalC keyTypeName []] []
      genRawKeyClause = do
          body <- [| iso (const $ cs @String @ByteString $(litE $ StringL keyBytes)) (const $(conE keyTypeName)) |]
          pure $ Clause [] (NormalB body) []
  rawKeyInst <- instanceD (pure []) (conT ''RawKey `appT` conT keyTypeName) [funD 'rawKey [genRawKeyClause]]
  isKeyInst <- instanceD (pure []) (conT ''IsKey `appT` conT keyTypeName `appT` conT namespaceName)
    [tySynInstD ''Value $ tySynEqn [conT keyTypeName, conT namespaceName] t]
  storeDecl <- storeDec storeName substoreName =<< t
  pure $ [dataDecl, rawKeyInst, isKeyInst] <> storeDecl

data KeyType =
    Var Name
  | List Name
  | Array Name
  | Map Name Name

parseKeyType :: Type -> KeyType
parseKeyType = \case
  AppT (ConT n) (ConT m) -> case nameBase n of
    "Var" -> Var m
    "List" -> List m
    "Array" -> Array m
    a -> error $ "Unrecognized KeyType " <> a <> ", expected Var, List, Array, Map."
  AppT (AppT (ConT n) (ConT m)) (ConT o) -> case nameBase n of
    "Map" -> Map m o
    a -> error $ "Unrecognized KeyType " <> a <> ", expected Var, List, Array, Map."
  _ -> error $ "Unrecognized Store type, expected (Var a), (List a), (Array a), (Map k v)."

mkKeyName :: KeyType -> Name
mkKeyName = mkName . \case
  Var n -> nameBase n <> "VarKey"
  Array n -> nameBase n <> "ArrayKey"
  List n -> nameBase n <> "ListKey"
  Map n m -> nameBase n <> nameBase m <> "MapKey"

getStoreNamespace
  :: Name
  -> Q Name
getStoreNamespace storeName = do
  info <- reify storeName
  case info of
    VarI _ t _ -> case t of
      AppT (ConT n) (ConT m) ->
        if nameBase n == "Store"
          then pure m
          else error "Unable to find Store Namespace, make sure store is of type (Store ns)"
      _ -> error "Unable to find Store Namespace, make sure store is of type (Store ns)"
    _ -> error "Unable to find Store Namespace, make sure store is of type (Store ns)"


storeDec :: Name -> String -> Type -> Q [Dec]
storeDec store substoreName t = do
  let sig = SigD (mkName substoreName) t
  val <- valD (varP $ mkName substoreName) (NormalB <$> storeBody) []
  pure [sig,val]
  where
    kt = parseKeyType t
    keyName = mkKeyName kt
    storeBody = case kt of
      Var _   -> [| makeVar $(conE keyName) $(varE store) |]
      List _  -> [| makeList $(conE keyName) $(varE store) |]
      Array _ -> [| makeArray $(conE keyName) $(varE store) |]
      Map _ _ -> [| makeMap $(conE keyName) $(varE store) |]
