-----------------------------------------------------------------------------
--
-- Module      :  Data.List.Util
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
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
