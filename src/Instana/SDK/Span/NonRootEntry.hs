{-# LANGUAGE DeriveGeneric #-}
module Instana.SDK.Span.NonRootEntry (NonRootEntry(..)) where


import           Data.Aeson              (Value)
import           Data.Text               (Text)
import           GHC.Generics

import           Instana.SDK.Internal.Id (Id)


data NonRootEntry =
  NonRootEntry
    { traceId   :: Id
    , spanId    :: Id
    , parentId  :: Id
    , spanType  :: Text
    , timestamp :: Int
    , label     :: Text
    , spanData  :: Value
    } deriving (Eq, Generic, Show)

