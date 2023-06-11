{-
You are not required to edit this file, but do feel free to take a look. You may add some tests yourself, if you think of new ones and are able to discern the format that they should be specified in.
-}

{-
Tasty is the testing library that is used to specify tests.
The backends "tasty-hunit" and "tasty-quickcheck" specify the way that unit tests and property tests (respectively) are written.
-}
{-# LANGUAGE LambdaCase #-}

import Control.Applicative (liftA2, liftA3)
import Control.Monad (replicateM, forM)
import Data.Bool (bool)
import Data.Function (on)
import Data.Maybe (mapMaybe)
import Data.Char (isAlpha, isUpper)
import System.Console.ANSI (clearScreen)
import System.Random (randomRIO)
import Data.List ((\\))
import Hurdle
import Hurdle.Command
import Hurdle.Match
import Hurdle.Words
import Test.Tasty
  ( TestTree (..),
    defaultMain,
    testGroup,
  )
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

instance Arbitrary Match where
  arbitrary = elements [minBound .. maxBound]

main :: IO ()
main = do
  clearScreen
  defaultMain $ testGroup "Tests"
    [ testNormalise,
      testIsValid,
      testParseCommand,
      testExactMatches,
      testRemoveExacts,
      testGetMatches,
      testMatchingAlgo,
      testEliminate,
      testEliminateAll,
      testNextGuess
    ]

testNormalise :: TestTree
testNormalise = testGroup "Q1: normalise"
  [ testCase "\"speak\"         => \"SPEAK\""
      $ normalise "speak" @?= "SPEAK",
    testCase "\"Hello, world!\" => \"HELLOWORLD\""
      $ normalise "Hello, world!" @?= "HELLOWORLD",
    testProperty "Works on random strings"
      $ property $ forAll (replicateM 50 arbitraryASCIIChar)
          $ \str -> all (liftA2 (&&) isUpper isAlpha) $ normalise str
  ]

testIsValid :: TestTree
testIsValid = testGroup "Q2: isValid"
    [ testCase "\"horse\"   => True " $ isValid "horse"   @?= True
    , testCase "\"horse?!\" => False" $ isValid "horse?!" @?= True
    , testCase "\"cheese\"  => False" $ isValid "cheese"  @?= False
    ]

testParseCommand :: TestTree
testParseCommand = testGroup "Q3: parseCommand"
    [ testCase "\"Letters\"       => ShowLetters"
        $ parseCommand "Letters"       @?= ShowLetters,

      testCase "\"fruit\"         => Guess \"FRUIT\""
        $ parseCommand "Fruit"         @?= Guess "FRUIT",

      testCase "\"Hello, world!\" => Guess \"HELLOWORLD\""
        $ parseCommand "Hello, world!" @?= Guess "HELLOWORLD",

      testCase "\"give up\"       => GiveUp"
        $ parseCommand "give up"       @?= GiveUp
    ]

testExactMatches :: TestTree
testExactMatches =
  testGroup "Q4: exactMatches" $
    let t a b c = testCase
          (concat [show a, ",", show b, " => ", show c])
          (exactMatches a b @?= c)
        x a b c = and $ zipWith3 x' c a b
        x' = (bool (/=) (==) . (\case {IsExact _ -> True; _ -> False}))
     in [ t "HORSE" "HEAVE"
            [ IsExact 'H'
            , IsNotExact 'O'
            , IsNotExact 'R'
            , IsNotExact 'S'
            , IsExact 'E'
            ]
        , t "CHEER" "REACH"
            [ IsNotExact 'C'
            , IsNotExact 'H'
            , IsNotExact 'E'
            , IsNotExact 'E'
            , IsNotExact 'R'
            ]
        , t "WORDS" "WORDS"
            [ IsExact 'W',
              IsExact 'O',
              IsExact 'R',
              IsExact 'D',
              IsExact 'S'
            ]
        , testProperty "Works on random examples" $
            property $ forAll (liftA2 (,)
              (normalise <$> elements guessList)
              (normalise <$> elements answerList)
              ) $ \(g,a) -> x g a (exactMatches g a)
        ]

testRemoveExacts :: TestTree
testRemoveExacts = testGroup "Q5: removeExacts"
  $ let
     count c = length . filter (==c)
     exacts = mapMaybe (\case { IsExact c -> Just c; _ -> Nothing })
    in
  [ testProperty "Output never contains letters that weren't in the input"
    $ property $ forAll (do
          w <- replicateM 5 $ elements ['A' .. 'Z']
          x <- forM w $ \c -> do
            e <- arbitrary
            if e
              then pure $ IsExact c
              else IsNotExact <$> elements ['A' .. 'Z']
          pure (x,w)
        ) $ \(x, w) -> let
          w' = removeExacts x w
        in all (\c -> ((<=) `on` count c) w' w) w
  , testProperty "The correct letters are removed"
    $ property $ forAll (do
          w <- replicateM 5 $ elements ['A' .. 'Z']
          x <- forM w $ \c -> do
            e <- arbitrary
            if e
              then pure $ IsExact c
              else IsNotExact <$> elements ['A' .. 'Z']
          pure (x,w)
        ) $ \(x, w) -> let
          w' = removeExacts x w
          in all (\c -> count c w == count c w' + count c (exacts x)) w
  ]

testGetMatches :: TestTree
testGetMatches = testGroup "Q6: getMatches"
  $ let t a b c =
          testCase
            (concat [show a, ",", show b, " => ", show c])
            (getMatches (exactMatches a b) (removeExacts (exactMatches a b) b) @?= c)
     in [ t "WORDS" "CATCH" [None, None, None, None, None],
          t "ORATE" "CHEAT" [None, None, Partial, Partial, Partial],
          t "EERIE" "BEERS" [Partial,Exact,Partial,None,None],
          t "BEERS" "EERIE" [None,Exact,Partial,Partial,None]
        ]

testMatchingAlgo :: TestTree
testMatchingAlgo = testGroup "Q7: matchingAlgo"
  $ let t a b c =
          testCase
            (concat [show a, ",", show b, " => ", show c])
            (matchingAlgo a b @?= c)
     in [ t "WORDS" "CATCH" [None, None, None, None, None],
          t "ORATE" "CHEAT" [None, None, Partial, Partial, Partial],
          t "EERIE" "BEERS" [Partial,Exact,Partial,None,None],
          t "BEERS" "EERIE" [None,Exact,Partial,Partial,None]
        ]

testEliminate :: TestTree
testEliminate = testGroup "Q8: eliminate"
  [ testCase "Spec example 1 (HORSE)"
    $ let
        m = [Exact, None, None, None, None]
        ws = ["HOUSE", "CHEER", "HAPPY"]
      in
        eliminate "HORSE" m ws @?= ["HAPPY"]
  , testCase "Spec example 2 (PLANT)"
    $ let
        m = [None, None, Exact, Partial, Partial]
        ws = ["TRAIN", "LAUGH", "STAIN"]
      in
        eliminate "PLANT" m ws @?= ["TRAIN", "STAIN"]
  , testCase "Spec example 3 (BRACE)"
    $ let
        m  = [None, Exact, None, Exact, None]
        ws = ["CRACK", "PRICK", "WHACK", "ROACH"]
      in
        eliminate "BRACE" m ws @?= ["PRICK"]
  , testProperty "Works on random examples"
    $ property $ forAllShrink (liftA3 (,,)
        (replicateM 5 $ elements ['A' .. 'Z'])
        (replicateM 5 $ elements [None, Partial, Exact])
        (sublistOf guessList)
      ) genericShrink $ \(a,m,gs) -> let a' = normalise a; e = eliminate a' m gs in 
          all    ((==m). matchingAlgo a') e
          && all ((/=m). matchingAlgo a') (gs \\ e)
  ]

testEliminateAll :: TestTree
testEliminateAll = testGroup "Q9: eliminateAll"
  [
    testProperty "Works on random examples"
    $ property $ forAll (choose (1,8) >>= flip replicateM (liftA2 (,)
        (replicateM 5 $ elements ['A' .. 'Z'])
        (replicateM 5 $ elements [None, Partial, Exact]))) $ \gs ->
          let e' = eliminateAll gs
              negated = map normalise guessList \\ e'
          in -- For every word still allowed, the matching algorithm produces identical results.
            all (\accepted -> all (\(g, m) -> m == matchingAlgo g accepted) gs) e' &&
            -- For every word rejected, there's some case where the matching algorithm produces different results.
            all (\rejected -> any (\(g, m) -> m /= matchingAlgo g rejected) gs) negated

  ]

testNextGuess :: TestTree
testNextGuess    = testGroup "Q10: nextGuess"
  [
    testCase "Successfully solves Hurdle"
      $ do
        let numTries = 300
        gs <- replicateM numTries $ do
          ans <- normalise . (answerList !!) <$> randomRIO (0, length answerList - 1)
          let x rs = let g = nextGuess rs
                     in if ans == nextGuess rs
                          then length rs + 1
                          else x (rs++[(g, matchingAlgo g ans)])
          pure $ x []
        let meanGuesses = fromIntegral (sum gs) / fromIntegral numTries
        putStrLn $ "Average guesses taken: " ++ show meanGuesses
  ]