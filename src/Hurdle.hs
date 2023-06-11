{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Hurdle where

import Hurdle.Command
import Hurdle.Match
import Data.Char ( toUpper, ord )
import Hurdle.Words ( guessList, answerList )
import Text.Read
import Data.List
import GHC.Read (list)


--------------------------------------------------------------------------------
-- This file is your complete submission for the first coursework of CS141.
-- 
-- u2109963 
--
-- Before starting on this file, ensure that you have read the specification IN
-- ITS ENTIRETY and that you understand everything that is required from a good
-- solution.
--------------------------------------------------------------------------------


-- | 1. Given an input guess, change it so that it is all upper case.        
--
-- This function takes every character in the given input and uses a guard to check if the letter is actually a character we wish to have in the final string.
-- At first I was going to generate the list with list comprehension techniques e.g (['a'..'z'] ++ map toUpper ['a' .. 'z'] However, I decided against this as storing the list locally meant the machine has 
-- to do less processing which means that the function will take less time to run for a given input. 
-- I am using `elem` *syntactic sugar* so that I can place the function between the parameters to make the code more readable. I use elem as it is the easiest way to check if a parameter is an
-- element of a list. It also returns a boolean which is required for a guard condition to be satisfied.
normalise :: String -> String
normalise input = [toUpper letter | letter <- input, letter `elem` alphabet]
alphabet = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

--------------------------------------------------------------------------------
-- | 2. A valid guess is a one which appears in `guessList`.                 
--
-- Within the function I use normalise which was the previous function I created. I use this to make sure that the word I am passing to the function is of the expected capitalised form and that no dissallowed 
-- characters are in the input.
-- I am using `elem` *syntactic sugar* so that I can place the function between the parameters to make the code more readable. I use elem as it is the easiest way to check if a parameter is an
-- element of a list. It also returns a boolean which is the expected return type for the function. 
isValid :: String -> Bool
isValid unvalidatedWord = normalise unvalidatedWord `elem` guessList




--------------------------------------------------------------------------------
-- | 3. Our program runs a little command line. Specific strings should be   
-- treated not as guesses, but as commands. See the specification for details.
--
-- This function takes in a input which could be un normalised. It then normalises the string and compares the string against given commands. If the string passed to the function is what 
-- is require to activate a command the corresponding command to the string is executed. If the string does not correspond to a command the function treates it as a guess.

-- The function is implemented using guards. I did this because guards are far more readable that If Else statements so it makes the function far easier to debug. The function also makes use of a where clause.
-- This was used to define the variable normalisedInput. I did this so that I do not have to keep recalulating normalise input. This means that the function can run faster as it has to do less calculations
-- per call. I initially tried to use pattern matching but realised that I had to normalise the input to make sure that the function worked for all instances of capitalisation. This meant that I was restricted to 
-- using the normalise function with selection.
parseCommand :: String -> Command
parseCommand input | normalisedInput  == "LETTERS" = ShowLetters
                   | normalisedInput == "GIVEUP" = GiveUp
                   | otherwise = Guess normalisedInput

                   where
                       normalisedInput = normalise input



--------------------------------------------------------------------------------
-- | 4. Part one of the matching algorithm finds the exact matches.
-- For each position, give back IsExact if the two characters are the 
-- same, or IsNotExact if they are different. 
-- Implement this using explicit recursion. If you can see a more elegant 
-- solution, describe it in your justification.

-- This function "loops" recrursively through each character in the guess and compares it to the letter at the corresponding index in the answer. If the letters are the same it appends
-- IsExact of the head of the list to a list. If the two characters do not align IsNotExact character is appeneded to the array.

-- The function also makes use of left side pattern matching as well. Initially I implemented the function making use of prelude's head and tail function. However, these must be 
-- performed multiple times in the function. In the case where i used head and tail I could have created a where clause to reduce the amount of computation the function required per call.
-- However, I decided to stick with using left side pattern matching as it reduced the amount of code I had to write and made for a more elegant solution.

-- Since all inputs to the function are normalised and validated I do not need to check if the inputted word is valid.

-- This function makes use of pattern matching on the input. This allows me to not 
-- This function is implemented using top level pattern matching to define the base case of the recusrive function. The base case is the empty list, this allows us to concatenate data to the head of the list after
-- the function reaches max depth.
-- The function is implemented using guards. I did this because guards are far more readable that If Else statements so it makes the function far easier to debug.
-- 
exactMatches :: String -> String -> [ExactMatch]
exactMatches [] [] = []
exactMatches (x:xs) (y:ys) | x == y = IsExact x : exactMatches xs ys
                           | otherwise = IsNotExact x : exactMatches xs ys



--------------------------------------------------------------------------------
-- | 5. We want to keep track of the "unused" characters in the answer. First, 
-- we use up all of the exact matches. This function takes the exact matches and 
-- the answer and gives back all the characters not already exactly matched.


-- This function works by recursing over all elements in the result of exactMatches and the characters and checking if the letter is an exaxt match to the answers character. If it is
-- the function append the character to the head to the list. Otherwise it does nothing, in both cases it recurses over the rest of the lists until they are both empty.
-- The function does this by making use of top level pattern matching to create the base case of the recursion.

-- The function is implemented using guards. I did this because guards are far more readable that If Else statements so it makes the function far easier to debug.

-- The function also makes use of left side pattern matching as well. Initially I implemented the function making use of prelude's head and tail function. However, these must be 
-- performed multiple times in the function. In the case where i used head and tail I could have created a where clause to reduce the amount of computation the function required per call
-- However, I decided to stick with using left side pattern matching as it reduced the amount of code I had to write and made for a more elegant solution.

removeExacts :: [ExactMatch] -> String -> String
removeExacts [] [] = []
removeExacts (x:xs) (y:ys) | IsExact y /= x = y : removeExacts xs ys
                           | otherwise = removeExacts xs ys



--------------------------------------------------------------------------------
-- | 6. Follow the algorithm in the specification to correctly return the list 
-- of character matches, given the result of exactMatches and any unused 
-- characters of the answer.
--

-- This function takes in the result of exactMatches and removeExacts. If the position in question was an exact match, then we return add Exact to the list, 
-- Otherwise, if the character in question appears in the remaining answer letters, then that position is a Partial match so we append partial to the list and delete the letter from
-- from the answers letters. To do this I make use of the elem function which is the easiest way to check if the character appears in the remanining answer letters
-- Otherwise the character must not appear in the string and a None should be added to the list. The function recurses over exactMatcges until it is empty.

-- To achieve the above logic i had to make use of left side pattern matching. This was neccessary as I had to be able to extract the character to be able to delete it in the case of
-- IsNotExact. Where the element at the position in exactMatches is a IsExact (character) I had to be able to generalise for all characters. Pattern matching allowed me to do this!

-- I could have use case x of. However, I believe, that style of patter matching is more crude and does not produce as neat/readable a solution.

-- The function is implemented using guards. I did this because guards are far more readable that If Else statements so it makes the function far easier to debug.

-- I am using `elem` *syntactic sugar* so that I can place the function between the parameters to make the code more readable. 


getMatches :: [ExactMatch] -> String -> [Match]

getMatches [] _ = []
getMatches (IsExact x : xs) removeExactsResult = Exact : getMatches xs removeExactsResult
getMatches (IsNotExact x : xs) removeExactsResult | x `elem` removeExactsResult =  Partial : getMatches xs (delete x removeExactsResult)
                                                  | otherwise = None : getMatches xs removeExactsResult




--------------------------------------------------------------------------------
-- | 7. Write the complete matching algorithm as a composition of the above 
-- three functions.
--

-- This function makes use of all the other functions I have defined to return a list of type Match

-- exactMatches guess answer must be processed multiple times in the same line I have used a where clause. This has allowed me to reduce the number of time the result of "matches"
-- must be calculated making the program more efficient.


matchingAlgo :: String -> String -> [Match]
matchingAlgo guess answer = getMatches matches (removeExacts matches answer)
                            where
                                matches = exactMatches guess answer


--------------------------------------------------------------------------------
-- | 8. Given a list of candidate words, remove those words which would not 
-- have generated the given match based on the guess that was made.
--
-- This function recurses over ever element in the guessList. It then runs the matchingAlgo on the guess and the current head of the guess list.
-- When we make a guess, we get back a particular matching; we can rule out any candidate words which would have given us a different matching for that guess. So when the result
-- of matchingAlgo is the same as the matches returned for the guess the word is appended to a list. Otherwise, the word is skipped over. 

-- The function stops recursing when the wordlist is empty. It does this by using top level pattern matching which is defined as returning the empty list when the wordlist is empty.

-- The function is implemented using guards. I did this because guards are far more readable that If Else statements so it makes the function far easier to debug.

-- The function also makes use of left side pattern matching on the element of the wordlist. This means the head/tail functions do not need to be used in conjunction with a where 
-- to reduce processing times making the function more efficient.


eliminate :: String -> [Match] -> [String] -> [String]
eliminate guess matches [] = []
eliminate guess matches (x:xs) | matchingAlgo guess x == matches = x : eliminate guess matches xs
                               | otherwise = eliminate guess matches xs

                



--------------------------------------------------------------------------------
-- | 9. Based on the whole history of the game so far, return only those words 
-- from `guessList` which might still be the hidden word.
--
-- This function returns the list of words based on the last guesses that are most likely to contain the answer. By reducing the size of the guess list each time we hone in on the 
-- actual answer to the puzzle

-- This function makes use of eliminate. The wordlist that is passed to eliminate is the result of the algorithm recursing on the tail of the infomation on all the next guesses until the 
-- list is empty. When the list is empty the guess list is returned so that the result of every eliminate before it can be removed from the guess list producing the final list which 
-- contains the guess the AI should make. 

-- The function also makes use of left side pattern matching on the element of the list (String,Match). This means the head/tail functions do not need to be used in conjunction with a 
-- where to reduce processing times making the function more efficient.

eliminateAll :: [(String, [Match])] -> [String]
eliminateAll [] = guessList
eliminateAll ((guess,match): xs) = eliminate guess match (eliminateAll xs)




--------------------------------------------------------------------------------
-- | 10. Using the above functions, write a function which produces a next guess 
-- based on the history of the game so far. 
--
-- nextGuess initialy guesses CRANE as according to: https://thesmartlocal.com/read/best-wordle-words/ it is the best first word to choose. The second guess in most cases will be the
-- word SLIPT as the website also referes to that being the second best word to choose. There are 3^5 combinations of matches I could get. I chose to choose some specific case that
-- were stastically common based on word frequency according to this video https://www.youtube.com/watch?v=v68zYyaEmEA and gave the word that should be chosen as a second guess.
-- If I had the time / sense. I would expand this list using some sort of function / a pregenerated hash map to avoid using guards which make looking an element up ineficcient.
-- The hash map would be used to look up certain permutation of a list of matches to choose the most optimal word for each stage of guessing. However, I do not have enough time to implement
-- this.

-- My function makes use of top level pattern matching to create the first guess and uses guards to check if the length of the array is 1. In which case the second guess is determined
-- by the the secondGuess function.

-- My nextGuess function also makes use of the head function from prelude as after the seond guess there is no real logic and I choose the head of the redcued list as a guess.
-- To be more efficient I would have to create the afore mentioned hash map to lookup a given set of matches for a word.

-- I make use of guards in the nextGuess function as it allows me to write neater code that an if then else statement.




nextGuess :: [(String, [Match])] -> String
nextGuess [] = "CRANE"
nextGuess guessMatch | length guessMatch == 1 = secondGuess guessMatch
                     | otherwise = head (eliminateAll guessMatch)
    

secondGuess :: [(String, [Match])] -> String

secondGuess ((word,match): xs) | match == [None,None,Partial,None,None]  = "TOILS"
                          | match == [None,None,None,None,Partial]       = "SPLIT"
                          | match == [None,Partial,None,None,Partial]    = "ROSIT"
                          | match == [None,None,None,None,Exact]         = "TOILS"
                          | match == [None,Partial,None,None,None]       = "SHOUT"
                          | match == [None,Partial,Partial,None,None]    = "PARTY"
                          | match == [None,None,None,Exact,None]         = "GOUTS"
                          | match == [None,None,None,Partial,Partial]    = "TIMON"
                          | match == [None,None,None,Partial,None]       = "PILOT"
                          | match == [Partial,None,None,None,None]       = "KUTIS"
                          | match == [None,Exact,None,None,None]         = "PILOT"
                          | match == [None,Partial,Partial,None,Partial] = "PATLY"
                          | otherwise                                    = "SLIPT"






                        
                     
