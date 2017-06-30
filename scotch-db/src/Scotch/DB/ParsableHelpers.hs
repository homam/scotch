{-# LANGUAGE
    OverloadedStrings
  , FlexibleInstances
  , UndecidableInstances
#-}

{-|
Module      : Scotch.DB.Types.ParsableHelpers
Description : OVERLAPPABLE instances for Scotty Parsable class.
-}
module Scotch.DB.ParsableHelpers (
)

where

import Web.Scotty
import qualified Data.Map as M
import Data.Text.Lazy (toLower, pack)

instance {-# OVERLAPPABLE #-} (Enum e, Read e, Show e, Bounded e) => Parsable e where
  parseParam txt =
    let dic = M.fromList $ map (\g -> (toLower . pack . show $ g, g)) [minBound ..]
    in case M.lookup (toLower txt) dic of
      Just gw -> Right gw
      Nothing -> Left "Unable to Parse."
