module Hurdle.Command where

{-
The Command data type tells the game code whether the user has made a guess,
asked it to print the letters keyboard, or wants to give up.
-}
data Command
    = Guess String 
    | GiveUp
    | ShowLetters
    deriving (Eq, Show)