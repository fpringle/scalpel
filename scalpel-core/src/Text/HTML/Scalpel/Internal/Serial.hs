{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK hide #-}
module Text.HTML.Scalpel.Internal.Serial (
    SerialScraper
,   SerialScraperT
,   inSerial
,   stepBack
,   stepNext
,   seekBack
,   seekNext
,   untilBack
,   untilNext
) where

import Text.HTML.Scalpel.Internal.Scrape
import Text.HTML.Scalpel.Internal.Select

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Except (MonadError)
import Control.Monad.Cont (MonadCont)
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Control.Monad.Writer (MonadWriter)
import Data.Bifunctor
import Data.Functor.Identity
import Data.List.PointedList (PointedList)
import Data.Maybe
import Prelude hiding (until)

import qualified Data.List.PointedList as PointedList
import qualified Data.Tree as Tree
import qualified Text.StringLike as TagSoup


-- | Serial scrapers operate on a zipper of tag specs that correspond to the
-- root nodes / siblings in a document.
--
-- Access to the zipper is always performed in a move-then-read manner. For this
-- reason it is valid for the current focus of the zipper to be just off either
-- end of list such that moving forward or backward would result in reading the
-- first or last node.
--
-- These valid focuses are expressed as Nothing values at either end of the
-- zipper since they are valid positions for the focus to pass over, but not
-- valid positions to read.
type SpecZipper str = PointedList (Maybe (TagSpec str))

-- | A 'SerialScraper' allows for the application of 'Scraper's on a sequence of
-- sibling nodes. This allows for use cases like targeting the sibling of a
-- node, or extracting a sequence of sibling nodes (e.g. paragraphs (\<p\>)
-- under a header (\<h2\>)).
--
-- Conceptually serial scrapers operate on a sequence of tags that correspond to
-- the immediate children of the currently focused node. For example, given the
-- following HTML:
--
-- @
--  \<article\>
--    \<h1\>title\</h1\>
--    \<h2\>Section 1\</h2\>
--    \<p\>Paragraph 1.1\</p\>
--    \<p\>Paragraph 1.2\</p\>
--    \<h2\>Section 2\</h2\>
--    \<p\>Paragraph 2.1\</p\>
--    \<p\>Paragraph 2.2\</p\>
--  \</article\>
-- @
--
-- A serial scraper that visits the header and paragraph nodes can be executed
-- with the following:
--
-- @
-- 'chroot' "article" $ 'inSerial' $ do ...
-- @
--
-- Each 'SerialScraper' primitive follows the pattern of first moving the focus
-- backward or forward and then extracting content from the new focus.
-- Attempting to extract content from beyond the end of the sequence causes the
-- scraper to fail.
--
-- To complete the above example, the article's structure and content can be
-- extracted with the following code:
--
-- @
-- 'chroot' "article" $ 'inSerial' $ do
--     title <- 'seekNext' $ 'text' "h1"
--     sections <- many $ do
--        section <- 'seekNext' $ text "h2"
--        ps <- 'untilNext' ('matches' "h2") (many $ 'seekNext' $ 'text' "p")
--        return (section, ps)
--     return (title, sections)
-- @
--
-- Which will evaluate to:
--
-- @
--  ("title", [
--    ("Section 1", ["Paragraph 1.1", "Paragraph 1.2"]),
--    ("Section 2", ["Paragraph 2.1", "Paragraph 2.2"]),
--  ])
-- @
type SerialScraper str a = SerialScraperT str Identity a

-- | Run a serial scraper transforming over a monad 'm'.
newtype SerialScraperT str m a =
    MkSerialScraper (StateT (SpecZipper str) (MaybeT m) a)
    deriving (Functor, Applicative, Alternative, Monad, MonadPlus, MonadFix, MonadFail,
              MonadIO, MonadCont, MonadError e, MonadReader r, MonadWriter w)

instance MonadTrans (SerialScraperT str) where
  lift = MkSerialScraper . lift . lift

instance MonadState s m => MonadState s (SerialScraperT str m) where
  get = MkSerialScraper (lift . lift $ get)
  put = MkSerialScraper . lift . lift . put

-- | Executes a 'SerialScraper' in the context of a 'Scraper'. The immediate
-- children of the currently focused node are visited serially.
inSerial :: (TagSoup.StringLike str, Monad m)
    => SerialScraperT str m a -> ScraperT str m a
inSerial (MkSerialScraper serialScraper) = MkScraper $ ReaderT scraper
  where
    scraper spec@(vec, root : _, ctx)
      | ctxInChroot ctx = evalStateT serialScraper
                                  (toZipper (vec, Tree.subForest root, ctx))
      | otherwise       = evalStateT serialScraper (toZipper spec)
    scraper _           = empty

    -- Create a zipper from the current tag spec by generating a new tag spec
    -- that just contains each root node in the forest.
    toZipper (vector, forest, context) =
        zipperFromList $ map ((vector, , context) . return) forest

-- | Creates a SpecZipper from a list of tag specs. This requires bookending the
-- zipper with Nothing values to denote valid focuses that are just off either
-- end of the list.
zipperFromList :: TagSoup.StringLike str => [TagSpec str] -> SpecZipper str
zipperFromList = PointedList.insertLeft Nothing
               . foldr (PointedList.insertLeft . Just)
                       (PointedList.singleton Nothing)

stepWith :: (TagSoup.StringLike str, Monad m)
         => (SpecZipper str -> Maybe (SpecZipper str))
         -> ScraperT str m b
         -> SerialScraperT str m b
stepWith moveList (MkScraper (ReaderT scraper)) = MkSerialScraper . StateT $
    \zipper -> do
        zipper' <- maybeT $ moveList zipper
        focus <- maybeT $ PointedList._focus zipper'
        value <- scraper focus
        return (value, zipper')

-- | Move the cursor back one node and execute the given scraper on the new
-- focused node.
stepBack :: (TagSoup.StringLike str, Monad m) => ScraperT str m a -> SerialScraperT str m a
stepBack = stepWith PointedList.previous

-- | Move the cursor forward one node and execute the given scraper on the new
-- focused node.
stepNext :: (TagSoup.StringLike str, Monad m)
    => ScraperT str m a -> SerialScraperT str m a
stepNext = stepWith PointedList.next

seekWith :: (TagSoup.StringLike str, Monad m)
         => (SpecZipper str -> Maybe (SpecZipper str))
         -> ScraperT str m b
         -> SerialScraperT str m b
seekWith moveList (MkScraper (ReaderT scraper)) = MkSerialScraper (StateT go)
    where
      go zipper = do zipper' <- maybeT $ moveList zipper
                     runScraper zipper' <|> go zipper'
      runScraper zipper = do
        focus <- maybeT $ PointedList._focus zipper
        value <- scraper focus
        return (value, zipper)

-- | Move the cursor backward until the given scraper is successfully able to
-- execute on the focused node. If the scraper is never successful then the
-- serial scraper will fail.
seekBack :: (TagSoup.StringLike str, Monad m)
    => ScraperT str m a -> SerialScraperT str m a
seekBack = seekWith PointedList.previous

-- | Move the cursor forward until the given scraper is successfully able to
-- execute on the focused node. If the scraper is never successful then the
-- serial scraper will fail.
seekNext :: (TagSoup.StringLike str, Monad m)
    => ScraperT str m a -> SerialScraperT str m a
seekNext = seekWith PointedList.next

untilWith :: (TagSoup.StringLike str, Monad m)
         => (SpecZipper str -> Maybe (SpecZipper str))
         -> (Maybe (TagSpec str) -> SpecZipper str -> SpecZipper str)
         -> ScraperT str m a
         -> SerialScraperT str m b
         -> SerialScraperT str m b
untilWith moveList appendNode (MkScraper (ReaderT until)) (MkSerialScraper scraper) =
  MkSerialScraper $ do
    inner <- StateT split
    lift (evalStateT scraper (appendNode Nothing inner))
    where
      split zipper =
        do zipper' <- maybeT $ moveList zipper
           spec <- maybeT $ PointedList._focus zipper'
           do until spec
              return (PointedList.singleton Nothing, zipper)
            <|> (first (appendNode (Just spec)) `fmap` split zipper')
         <|> return (PointedList.singleton Nothing, zipper)

-- | Create a new serial context by moving the focus backward and collecting
-- nodes until the scraper matches the focused node. The serial scraper is then
-- executed on the collected nodes.
untilBack :: (TagSoup.StringLike str, Monad m)
          => ScraperT str m a -> SerialScraperT str m b -> SerialScraperT str m b
untilBack = untilWith PointedList.previous PointedList.insertRight

-- | Create a new serial context by moving the focus forward and collecting
-- nodes until the scraper matches the focused node. The serial scraper is then
-- executed on the collected nodes.
--
-- The provided serial scraper is unable to see nodes outside the new restricted
-- context.
untilNext :: (TagSoup.StringLike str, Monad m)
          => ScraperT str m a -> SerialScraperT str m b -> SerialScraperT str m b
untilNext = untilWith PointedList.next PointedList.insertLeft

maybeT :: Monad m => Maybe a -> MaybeT m a
maybeT = MaybeT . return
