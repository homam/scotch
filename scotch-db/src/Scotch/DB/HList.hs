{-# LANGUAGE
    OverloadedStrings
  , FlexibleInstances
  , GADTs
  , TypeFamilies
  , TypeOperators
  , DataKinds
  , PolyKinds
  , KindSignatures
  , ConstraintKinds
  , UndecidableInstances
#-}

module Scotch.DB.HList (
    HList(..)
  , showHList
  , toRowHList
)
where

import Database.PostgreSQL.Simple.ToField (Action)
import Database.PostgreSQL.Simple.ToRow (toRow, ToRow)
import Database.PostgreSQL.Simple.FromRow (fromRow, FromRow, field)
import GHC.Exts (Constraint)

infixr 5 :::

data HList (ts :: [ * ]) where
  Nil :: HList '[]
  (:::) :: t -> HList ts -> HList (t ': ts)

type family Map (f :: a -> b) (xs :: [a]) :: [b]
type instance Map f '[] = '[]
type instance Map f (x ': xs) = f x ': Map f xs

type family Constraints (cs :: [Constraint]) :: Constraint
type instance Constraints '[] = ()
type instance Constraints (c ': cs) = (c, Constraints cs)

type AllHave (c :: k -> Constraint) (xs :: [k]) = Constraints (Map c xs)

showHList :: AllHave Show xs => HList xs -> [String]
showHList Nil = []
showHList (x ::: xs) = show x : showHList xs

instance AllHave Show xs => Show (HList xs) where
  show = show . showHList

toRowHList :: AllHave ToRow xs => HList xs -> [Action]
toRowHList Nil = []
toRowHList (x ::: xs) = toRow x ++ toRowHList xs

instance AllHave ToRow xs => ToRow (HList xs) where
  toRow = toRowHList

-- Example
-- >>>
-- example1 :: HList '[Bool, String , Double , ()]
-- example1 = True ::: "foo" ::: 3.14 ::: () ::: Nil
