{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_HADDOCK hide #-}

module Text.HTML.Scalpel.Internal.Scrape.Error
  ( ScrapeError
  , singleError
  , multiErrors
  , mkError
  , flattenErrors
  , flattenErrorsRecursive
  , renderScraperErrorCompact
  , renderScraperErrorPretty
  )
where

import Control.Exception
import qualified Data.Text as T
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

data ScrapeError where
  SingleError :: T.Text -> Maybe Int -> ScrapeError
  MultiErrors :: [ScrapeError] -> ScrapeError

singleError :: T.Text -> Maybe Int -> ScrapeError
singleError = SingleError

multiErrors :: [ScrapeError] -> ScrapeError
multiErrors = flattenErrors . MultiErrors

flattenErrors :: ScrapeError -> ScrapeError
flattenErrors = \case
  s@SingleError {} -> s
  MultiErrors [e] -> e
  MultiErrors errs ->
    let flatErrs = filter (/= MultiErrors []) errs
        mFlatErrs = MultiErrors flatErrs
    in if flatErrs == errs
        then mFlatErrs
        else flattenErrors mFlatErrs

flattenErrorsRecursive :: ScrapeError -> ScrapeError
flattenErrorsRecursive = \case
  s@SingleError {} -> s
  MultiErrors [e] -> flattenErrors e
  MultiErrors errs ->
    let flatErrs = filter (/= MultiErrors []) $ flattenErrors <$> errs
        mFlatErrs = MultiErrors flatErrs
    in if flatErrs == errs
        then mFlatErrs
        else flattenErrors mFlatErrs

deriving instance Generic ScrapeError

deriving instance Show ScrapeError

deriving instance Eq ScrapeError

instance NFData ScrapeError

mkError :: T.Text -> ScrapeError
mkError = flip SingleError Nothing

renderScraperErrorCompact :: ScrapeError -> T.Text
renderScraperErrorCompact = \case
  SingleError msg mbPos -> renderSingleError msg mbPos
  MultiErrors errs -> "[" <> T.intercalate ", " (renderScraperErrorCompact <$> errs) <> "]"

renderSingleError :: T.Text -> Maybe Int -> T.Text
renderSingleError msg Nothing = msg
renderSingleError msg (Just pos) = msg <> " (position " <> T.pack (show pos) <> ")"

{-
MultiErrors [SingleError "e1" (Just 0), MultiErrors [MultiErrors [], SingleError "e2" Nothing]]
=>
[ e1 (position 0)
, [ []
  , e1
  ]
]
-}
renderScraperErrorPretty :: ScrapeError -> T.Text
renderScraperErrorPretty = render 0
  where
    render :: Int -> ScrapeError -> T.Text
    render indent = \case
      SingleError msg mbPos -> renderSingleError msg mbPos
      MultiErrors [] -> "[]"
      MultiErrors errs ->
        T.intercalate "\n"
          . (<> [spaces <> "]"])
          . fmap (\(start, stuff) -> start <> " " <> stuff)
          . zip ("[" : repeat (spaces <> ","))
          $ render (indent + 2)
            <$> errs
      where
        spaces = T.replicate indent " "

instance Exception ScrapeError where
  displayException = T.unpack . renderScraperErrorCompact

instance Semigroup ScrapeError where
  MultiErrors e1 <> MultiErrors e2 = MultiErrors $ e1 <> e2
  e1 <> MultiErrors e2 = MultiErrors $ e1 : e2
  MultiErrors e1 <> e2 = MultiErrors $ e1 <> [e2]
  e1 <> e2 = MultiErrors [e1, e2]

instance Monoid ScrapeError where
  mempty = MultiErrors []
  mappend = (<>)
