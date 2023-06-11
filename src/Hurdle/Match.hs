module Hurdle.Match where

{- 
The final outcome of the matching algorithm is one Match
per place in the word.
-}
data Match = None | Partial | Exact
  deriving (Show, Eq, Ord, Enum, Bounded)

{-
This is the result of step one of the matching algorithm, which
returns one Exact for each place, telling us whether that place
is an exact match or not.
-}
data ExactMatch = IsExact Char | IsNotExact Char
  deriving (Show, Eq, Ord)