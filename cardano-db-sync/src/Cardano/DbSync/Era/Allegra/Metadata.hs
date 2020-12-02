{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.DbSync.Era.Allegra.Metadata
  ( fromAllegraMetadata
  , metadataValueToJsonNoSchema
  ) where

import           Cardano.Prelude

import           Cardano.Api.Shelley (TxMetadataValue (..))

-- import qualified Cardano.Ledger.Core as ShelleyMa
import qualified Cardano.Ledger.ShelleyMA.Metadata as ShelleyMa

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Text as Aeson.Text
import qualified Data.ByteString.Base16 as Base16
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Lazy as Text.Lazy
import           Data.Tuple.Extra (both)
import qualified Data.Vector as Vector

import           Ouroboros.Consensus.Cardano.Block (StandardAllegra)

import qualified Shelley.Spec.Ledger.MetaData as Shelley

-- This module should not even exist. The only reason it does is because functionality
-- that was in cardano-node commit 0dc6efa467a0fdae7aba7c5bcd5c657e189c8f19 and being
-- used here in db-sync was drastically changed and then the changed version was not
-- exported.

fromAllegraMetadata :: ShelleyMa.Metadata StandardAllegra -> Map Word64 TxMetadataValue
fromAllegraMetadata (ShelleyMa.Metadata mdMap _scripts) =
    Map.map fromAllegraMetaDatum mdMap
  where
    fromAllegraMetaDatum :: Shelley.MetaDatum -> TxMetadataValue
    fromAllegraMetaDatum smd =
      case smd of
        Shelley.I x -> TxMetaNumber x
        Shelley.B x -> TxMetaBytes  x
        Shelley.S x -> TxMetaText   x
        Shelley.List xs -> TxMetaList $ map fromAllegraMetaDatum xs
        Shelley.Map xs -> TxMetaMap $ map (both fromAllegraMetaDatum) xs


metadataValueToJsonNoSchema :: TxMetadataValue -> Aeson.Value
metadataValueToJsonNoSchema = conv
  where
    conv :: TxMetadataValue -> Aeson.Value
    conv (TxMetaNumber n) = Aeson.Number (fromInteger n)
    conv (TxMetaBytes bs) = Aeson.String (bytesPrefix
                                       <> Text.decodeLatin1 (Base16.encode bs))

    conv (TxMetaText txt) = Aeson.String txt
    conv (TxMetaList  vs) = Aeson.Array (Vector.fromList (map conv vs))
    conv (TxMetaMap  kvs) = Aeson.object
                              [ (convKey k, conv v)
                              | (k, v) <- kvs ]

    -- Metadata allows any value as a key, not just string as JSON does.
    -- For simple types we just convert them to string dirctly.
    -- For structured keys we render them as JSON and use that as the string.
    convKey :: TxMetadataValue -> Text
    convKey (TxMetaNumber n) = Text.pack (show n)
    convKey (TxMetaBytes bs) = bytesPrefix
                            <> Text.decodeLatin1 (Base16.encode bs)
    convKey (TxMetaText txt) = txt
    convKey v                = Text.Lazy.toStrict
                             . Aeson.Text.encodeToLazyText
                             . conv
                             $ v

-- | JSON strings that are base16 encoded and prefixed with 'bytesPrefix' will
-- be encoded as CBOR bytestrings.
bytesPrefix :: Text
bytesPrefix = "0x"
