{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : Instana.SDK.Span.SpanData
Description : A type for span data, that is, the content in span.data.
-}
module Instana.SDK.Span.SpanData
  ( SpanData (SpanData)
  , Annotation (..)
  , AnnotationValue (..)
  , empty
  , listAnnotation
  , listValue
  , merge
  , nullAnnotation
  , objectAnnotation
  , optionalAnnotation
  , optionalValue
  , simpleAnnotation
  , simpleValue
  , singleAnnotation
  ) where


import           Data.Aeson                (KeyValue, ToJSON, Value, (.=))
import qualified Data.Aeson                as Aeson
import qualified Data.List                 as L
import qualified Data.Maybe                as Maybe
import           Data.Text                 (Text)
import qualified Data.Vector               as V
import           GHC.Generics

import           Instana.SDK.Internal.Util ((|>))


-- |Represents span.data, that is, the tree of free-form annotatiations for a
-- span.
data SpanData =
  SpanData [Annotation]
  deriving (Eq, Generic, Show)


instance ToJSON SpanData where
  toJSON :: SpanData -> Value
  toJSON (SpanData annotations) =
    Aeson.object $
      map annotationToJson annotations
      |> Maybe.catMaybes


-- |Creates empty span data.
empty :: SpanData
empty = SpanData []


-- |Merges the given annotation into the given span data. If there is a conflict
-- between existing annotations and a new annotation (that is, both are at the
-- same path of keys), this will be resolved as follows:
-- - If both the existing and the new annotation are list annotations, they will
--   be merged. Duplicates are allowed (that is, duplicate values will not be
--   removed).
-- - Otherwise, if the existing annotation or the new annotation is a single
--   annotation, the new annotation will overwrite the existing one.
-- - If both the existing and the new annotation are object annotations, they
--   will be merged by applying these rules recursively.
merge :: Annotation -> SpanData -> SpanData
merge newAnnotation (SpanData existingAnnotations) =
  SpanData $ mergeAnnotation newAnnotation existingAnnotations


mergeAnnotation :: Annotation -> [Annotation] -> [Annotation]
mergeAnnotation newAnnotation existingAnnotations =
  let
    key =
      getKey newAnnotation
    existingAnnotationForKeyMaybe =
      L.find ((== key) . getKey) existingAnnotations
  in
  case existingAnnotationForKeyMaybe of
    Nothing ->
      -- The new key does not exist yet, simply add it.
      existingAnnotations ++ [newAnnotation]
    Just existingAnnotationForKey ->
      let
        annotationsWithoutPrevious =
          (L.delete existingAnnotationForKey existingAnnotations)
      in
      case (existingAnnotationForKey, newAnnotation) of
        (Single _ (List values1), Single _ (List values2)) ->

          -- The key exists already and both the existing and the new annotation
          -- are lists. Merge both list values into one list annotation.
          annotationsWithoutPrevious ++
            [Single key $ List $ values1 ++ values2]

        (Single _ _, _) ->

          -- The key exists already but was previously a single value. We
          -- cannot merge the new annotation into the single value annotation,
          -- overwrite it.
          annotationsWithoutPrevious ++ [newAnnotation]

        (_, Single _ _) ->

          -- The key exists already but the new value is single value. We
          -- cannot merge the new annotation into an existing annotation,
          -- overwrite it.
          annotationsWithoutPrevious ++ [newAnnotation]

        (Object _ previousChildren, Object _ newChildren) ->

          -- The key exists already and both the existing and the new annotation
          -- are object annotations. We merge both structures by recursively
          -- merging all child annotations from the new object annotation with
          -- the child annotations from the existing object annotation.
          let
            mergedChildAnnotations =
              L.foldr
                mergeAnnotation
                previousChildren -- starting value for foldr
                newChildren      -- the list we iterate over
          in
          annotationsWithoutPrevious ++ [Object key mergedChildAnnotations]


-- |Represents a single item in span.data, either an object with child
-- annotations or a single annotation (String, Int, Boolean, List, Maybe etc.)
data Annotation =
  -- |Similar to the type Data.Aeson.Types.Object, an object annotation can hold
  -- multiple child annotations. In
  -- { "http":
  --   { "url": "http://localhost:8080"
  --   , "method": "GET"
  --   , "headers": [("X-Header-1", "value 1), ("X-Header-2", "value 2)]
  --   }
  -- }
  -- the "http" key would be an Object annotation.
  Object Text [Annotation]
  -- |Somewhat similar to Data.Aeson.Types.Pair, a single annotation holds one
  -- single value (String, Number, Boolean, List, Maybe etc.). In the example
  -- above, "url", "method" and headers would be Single annotations.
  | Single Text AnnotationValue
  deriving (Eq, Generic, Show)


-- |Converts a single annotation to a Aeson.KeyValue. An annotation on its own
-- cannot be converted into an Aeson.Value, since it needs to be  wrapped in
-- another Object annotationa and ultimately in SpanData. This implementation
-- will also take care of removing optional annotations that are Nothing.
annotationToJson :: forall kv . KeyValue kv => Annotation -> Maybe kv
annotationToJson annotation =
  case annotation of
    Object key childAnnotations ->
      let
        object :: Value
        object =
          Aeson.object $
            map annotationToJson childAnnotations |> Maybe.catMaybes
        keyValue :: kv
        keyValue = (key .= object)
      in
      Just keyValue

    Single _ (Optional Nothing) ->
      -- drop optional annotations that are Nothing
      Nothing

    Single key annotationValue ->
      let
        pair :: kv
        pair = (key .= (Aeson.toJSON annotationValue))
      in
      Just pair


-- |Retrieves the key from an annotation.
getKey :: Annotation -> Text
getKey (Object key _) = key
getKey (Single key _) = key


-- |Creates a new object annotation. Similar to the type
-- Data.Aeson.Types.Object, an object annotation can hold multiple child
-- annotations. In
-- { "http":
--   { "url": "http://localhost:8080"
--   , "method": "GET"
--   }
-- }
-- the "http" key would be an Object annotation.
objectAnnotation :: Text -> [Annotation] -> Annotation
objectAnnotation key children = Object key children


-- |Creates a single annotation. Somewhat similar to Data.Aeson.Types.Pair, a
-- single annotation holds one value
-- (String, Number, Boolean, List, Maybe etc.).
--
-- The convenience functions simpleAnnotation, listAnnotation, and
-- optionalAnnotation allow creating a single annotation without creating the
-- AnnotationValue explicitly.
singleAnnotation :: Text -> AnnotationValue -> Annotation
singleAnnotation key value = Single key value


-- |Creates a simple annotation, which holds one primitive value (String,
-- Number, Boolean, etc.).
simpleAnnotation :: ToJSON a => Text -> a -> Annotation
simpleAnnotation key = (Single key) . simpleValue


-- |Creates a list annotation, which holds a list of items. For list
-- annotations, consecutive merges with the same key will add to the list
-- instead of overwriting previous values.
listAnnotation :: ToJSON a => Text -> [a] -> Annotation
listAnnotation key = (Single key) . listValue


-- |Creates an optional annotation, which holds a Maybe. If an optional
-- annotation holds a Nothing value, it will be ommitted when SpanData is
-- encoded to JSON.
optionalAnnotation :: ToJSON a => Text -> Maybe a -> Annotation
optionalAnnotation key = (Single key) . optionalValue


-- |A convenience function to create an optional annotation that holds a
-- Nothing.
nullAnnotation :: Text -> Annotation
nullAnnotation key = Single key $ Optional Nothing


-- |Represents the value of a span.data item.
data AnnotationValue =
  Simple Value
  -- |For list annotations consecutive merges with the same key will add to the
  -- list instead of overwriting previous values.
  | List [Value]
  -- |If an annotation is marked as optional, it will be ommitted from the
  -- encoded JSON if it is Nothing.
  | Optional (Maybe Value)
  deriving (Eq, Generic, Show)


instance ToJSON AnnotationValue where
  toJSON :: AnnotationValue -> Value
  toJSON annotation =
    case annotation of
      Simple value ->
        value
      List values ->
        Aeson.Array $ V.fromList values
      Optional maybeValue ->
        case maybeValue of
          Just value -> value
          -- Optional Nothing is actually already dropped in annotationToJson.
          Nothing    -> Aeson.Null


-- |Creates the value part of a simple annotation, that is, a primitive value
-- (String, Number, Boolean, etc.). You might want to use simpleAnnotation
-- directly instead of creating the simple value beforehand.
simpleValue :: ToJSON a => a -> AnnotationValue
simpleValue =
  Simple . Aeson.toJSON


-- |Creates the value part of a list annotation. You might want to use
-- listAnnotation directly instead of creating the list value beforehand.
listValue :: ToJSON a => [a] -> AnnotationValue
listValue =
  List . map Aeson.toJSON


-- |Creates the value part of an optional annotation. You might want to use
-- optionalAnnotation directly instead of creating the optional value
-- beforehand.
optionalValue :: ToJSON a => Maybe a -> AnnotationValue
optionalValue value =
  Optional $ Aeson.toJSON <$> value

