{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# OPTIONS_HADDOCK hide #-}

module Text.HTML.Scalpel.Internal.Select.Combinators
  ( (//)
  , (@:)
  , (@=)
  , (@=~)
  , atDepth
  , hasClass
  , match
  , notP
  )
where

import qualified Data.Text as T
import Text.HTML.Scalpel.Internal.Select.Types
import qualified Text.Regex.Base.RegexLike as RE
import qualified Text.StringLike as TagSoup

{- | The '@:' operator creates a 'Selector' by combining a 'TagName' with a list
 of 'AttributePredicate's.
-}
(@:) :: TagName -> [AttributePredicate] -> Selector
(@:) tag attrs = MkSelector [(toSelectNode tag attrs, defaultSelectSettings)]

infixl 9 @:

{- | The '@=' operator creates an 'AttributePredicate' that will match
 attributes with the given name and value.

 If you are attempting to match a specific class of a tag with potentially
 multiple classes, you should use the 'hasClass' utility function.
-}
(@=) :: AttributeName -> String -> AttributePredicate
(@=) key value = anyAttrPredicate $ \(attrKey, attrValue) ->
  matchKey key attrKey
    && TagSoup.fromString value == attrValue

infixl 6 @=

{- | The '@=~' operator creates an 'AttributePredicate' that will match
 attributes with the given name and whose value matches the given regular
 expression.
-}
(@=~) ::
  RE.RegexLike re String =>
  AttributeName ->
  re ->
  AttributePredicate
(@=~) key re = anyAttrPredicate $ \(attrKey, attrValue) ->
  matchKey key attrKey
    && RE.matchTest re (TagSoup.toString attrValue)

infixl 6 @=~

{- | The 'atDepth' operator constrains a 'Selector' to only match when it is at
 @depth@ below the previous selector.

 For example, @"div" // "a" `atDepth` 1@ creates a 'Selector' that matches
 anchor tags that are direct children of a div tag.
-}
atDepth :: Selector -> Int -> Selector
atDepth (MkSelector xs) depth = MkSelector (addDepth xs)
  where
    addDepth [] = []
    addDepth [(node, settings)] =
      [ (node, settings {selectSettingsDepth = Just depth})
      ]
    addDepth (x : xs) = x : addDepth xs

infixl 6 `atDepth`

{- | The '//' operator creates an 'Selector' by nesting one 'Selector' in
 another. For example, @"div" // "a"@ will create a 'Selector' that matches
 anchor tags that are nested arbitrarily deep within a div tag.
-}
(//) :: Selector -> Selector -> Selector
(//) a b = MkSelector (as ++ bs)
  where
    (MkSelector as) = a
    (MkSelector bs) = b

infixl 5 //

{- | The classes of a tag are defined in HTML as a space separated list given by
 the @class@ attribute. The 'hasClass' function will match a @class@ attribute
 if the given class appears anywhere in the space separated list of classes.
-}
hasClass :: String -> AttributePredicate
hasClass clazz = anyAttrPredicate hasClass'
  where
    hasClass' (attrName, classes)
      | "class" == TagSoup.toString attrName = textClass `elem` classList
      | otherwise = False
      where
        textClass = TagSoup.castString clazz
        textClasses = TagSoup.castString classes
        classList = T.split (== ' ') textClasses

-- | Negates an 'AttributePredicate'.
notP :: AttributePredicate -> AttributePredicate
notP (MkAttributePredicate p) = MkAttributePredicate $ not . p

{- | The 'match' function allows for the creation of arbitrary
 'AttributePredicate's. The argument is a function that takes the attribute
 key followed by the attribute value and returns a boolean indicating if the
 attribute satisfies the predicate.
-}
match :: (String -> String -> Bool) -> AttributePredicate
match f = anyAttrPredicate $ \(attrKey, attrValue) ->
  f (TagSoup.toString attrKey) (TagSoup.toString attrValue)
