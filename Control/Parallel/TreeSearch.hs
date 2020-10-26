-- |
-- Module      : Control.Parallel.TreeSearch
-- Copyright   : Fabian Reck, Sebastian Fischer
-- License     : PublicDomain
--
-- Maintainer  : Niels Bunkenburg (nbu@informatik.uni-kiel.de)
-- Stability   : experimental
-- Portability : portable
--
-- This Haskell library provides an implementation of parallel search
-- based on the search tree provided by the package tree-monad.
--
module Control.Parallel.TreeSearch ( parSearch ) where

import           Control.Monad.SearchTree
import           Control.Parallel

-- | Enumerate the leaves of a @SearchTree@ using parallel depth-first search.
parSearch :: SearchTree a  -- ^ tree to search
          -> [a]           -- ^ lazy list of leaves

parSearch None = []
parSearch (One x) = [x]
parSearch (Choice l r) = rs `par` (parSearch l ++ rs)
 where
  rs = parSearch r
