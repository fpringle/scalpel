{- |
 Scalpel core provides a subset of the
 <https://hackage.haskell.org/package/scalpel scalpel> web scraping library
 that is intended to have lightweight dependencies and to be free of all
 non-Haskell dependencies.

 Notably this package does not contain any networking support. Users who
 desire a batteries include solution should depend on @scalpel@ which does
 include networking support instead of @scalpel-core@.

 More thorough documentation including example code can be found in the
 documentation of the <https://hackage.haskell.org/package/scalpel scalpel>
 package.
-}
module Text.HTML.Scalpel.Core
  ( -- * Selectors
    Selector
  , AttributePredicate
  , AttributeName (..)
  , TagName (..)
  , tagSelector
  , textSelector

    -- ** Wildcards
  , anySelector

    -- ** Tag combinators
  , (//)
  , atDepth

    -- ** Attribute predicates
  , (@:)
  , (@=)
  , (@=~)
  , hasClass
  , notP
  , match

    -- * Scrapers
  , Scraper
  , ScraperT
  , ScrapeError
  , singleError
  , multiErrors
  , renderScraperErrorCompact
  , renderScraperErrorPretty
  , flattenErrors
  , flattenErrorsRecursive

    -- ** Primitives
  , attr
  , attrs
  , html
  , htmls
  , innerHTML
  , innerHTMLs
  , text
  , texts
  , chroot
  , chroots
  , position
  , matches

    -- ** Executing scrapers
  , scrape
  , scrapeT
  , scrapeStringLike
  , scrapeStringLikeT

    -- * Serial Scraping
  , SerialScraper
  , SerialScraperT
  , inSerial

    -- ** Primitives
  , stepNext
  , stepBack
  , seekNext
  , seekBack
  , untilNext
  , untilBack
  )
where

import Text.HTML.Scalpel.Internal.Scrape
import Text.HTML.Scalpel.Internal.Scrape.StringLike
import Text.HTML.Scalpel.Internal.Select.Combinators
import Text.HTML.Scalpel.Internal.Select.Types
import Text.HTML.Scalpel.Internal.Serial
