-----------------------------------------------------------------------------
--
-- Module      :  Data.List.Util
-- Copyright   :  (c) 2012-13 Brian W Bush
-- License     :  MIT
--
-- Maintainer  :  Brian W Bush <b.w.bush@acm.org>
-- Stability   :  Stable
-- Portability :  Portable
--
-- | Miscellaneous functions for manipulating lists.
--
-----------------------------------------------------------------------------


module Data.List.Util (
  separate
) where


-- | The 'separate' function splits a list into fragments separated by a given element. 
separate :: Eq a =>
            a     -- ^ The separating element 
         -> [a]   -- ^ The list to be separated
         -> [[a]] -- ^ The lists between the separating element
separate _ [] = []
separate a s =
  let
    (l, s') = break (== a) s
  in
    l : case s' of
          [] -> []
          [a] -> [[]]
          (_:s'') -> separate a s''
