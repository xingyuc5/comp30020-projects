--  File     : project1.hs
--  Author   : Xingyu Chen
--  Purpose  : An implementation of the card guessing game

-- | This code implements a card guessing game.
-- It contains three functions to complete the whole simulation of games|

module Proj1 (feedback, initialGuess, nextGuess, GameState) where
import Card
import Data.List

-- Gamestate is the remaining possible answer of cards, which is a list of lists
-- of cards
type GameState = [[Card]]

-- Take a list of cards as answer and a list of cards as guess, compare them
-- and return the results
-- Input xs: list of cards as answer
--       ys: list of cards as guess
-- Output (correctCard, lowerRank, correctRank, higerRank, correctSuit):
--       a 5-tuple containing information about comparison of answer and guess
--       correctCard:
--       lowerRank
--       correctRank:
--       higherRank:
--       correctSuit:
feedback :: [Card] -> [Card] -> (Int, Int, Int, Int, Int)
feedback xs ys
    = (correctCard, lowerRank, correctRank, higherRank, correctSuit)
        where
            correctCard = length [(Card s1 r1) | (Card s1 r1) <- xs, (Card s2 r2) <- ys, (Card s1 r1) == (Card s2 r2)]

            lowerRank = length [(Card s1 r1) | (Card s1 r1) <- xs,
                let rMin = minimum [r2 | (Card s2 r2) <- ys], r1 < rMin]

            correctRank = length (nubBy (\(x1, y1) (x2, y2) -> x1 == x2 || y1 == y2)
                [((Card s1 r1), (Card s2 r2)) | (Card s1 r1) <- xs,
                (Card s2 r2) <- ys, r1 == r2])

            higherRank = length [(Card s1 r1) | (Card s1 r1) <- xs,
                let rMax = maximum [r2 | (Card s2 r2) <- ys], r1 > rMax]

            correctSuit = length (nubBy (\(x1, y1) (x2, y2) -> x1 == x2 || y1 == y2)
                [((Card s1 r1), (Card s2 r2)) | (Card s1 r1) <- xs,
                (Card s2 r2) <- ys, s1 == s2])

-- Make an initial guess of cards given the number of cards in answer
-- Input Int x
initialGuess :: Int -> ([Card], GameState)
initialGuess x
    = (guess, gameState)
    where gameState = combinations x allCards
              where combinations :: Int -> [a] -> [[a]]
                    combinations 0 _ = [[]]
                    combinations n xs = [ xs !! i : x | i <- [0..(length xs)-1]
                      , x <- combinations (n-1) (drop (i+1) xs) ]
                    allCards = [(Card Club R2)..(Card Spade Ace)]

          guess = [Card s r | a <- [1..x],
              let s = suits !! ((a-1) `mod` suitCounts),
              let r = ranks !! (13 `div` (x+1) * a)]
                  where suits =  [Club ..]  -- "random" suit, different
                        ranks = [R2 ..]     -- number separated apart
                        suitCounts = 4

--(correctCard, lowerRank, correctRank,higherRank, correctSuit)
-- 1. Using received feedback and previous guess to reduce possible answers
-- 2. Using reduced possible answers to determine next guess
nextGuess :: ([Card], GameState) -> (Int, Int, Int, Int, Int) ->
    ([Card], GameState)
nextGuess ([], []) (0,0,0,0,0) = ([], [])
nextGuess (oldGuess, oldGameState) previousFeedback
    = let newGameState = [guess | guess <- oldGameState, feedback guess oldGuess == previousFeedback]
      in (findNextGuess newGameState oldGuess, newGameState)
          where findNextGuess :: GameState -> [Card] -> [Card]
                findNextGuess [[]] _ = []
                findNextGuess gameState oldGuess
                    = snd (minimum ([(evaluate x gameState, x) | x <- gameState]))
                           where evaluate :: [Card] -> GameState -> Int
                                 evaluate guessCandidate possibleAnswers
                                    -- = let sizes = [feedback x guessCandidate | x <- possibleAnswers]

                                    -- = let sizes = map (length) (group (sort [feedback x guessCandidate | x <- possibleAnswers]))
                                      in sum (map (^2) sizes) `div` sum sizes


-- nextGuess (oldGuess, oldGameState) previousFeedback = (newGuess, newGameState)
--     where newGameState = [guess | guess <- oldGameState, feedback guess oldGuess == previousFeedback]
--
--           newGuess = snd (minimum (zip [evaluate x (newGameState \\ [x]) | x <- newGameState] newGameState))
--                   where evaluate :: [Card] -> GameState -> Int
--                         evaluate guessCandidate possibleAnswers
--                           = sum (map (^2) sizes) `div` sum sizes
--                               where sizes = map (length) (group (sort [feedback x guessCandidate | x <- possibleAnswers]))







    -- where newGameState = filteredCC `intersect` filteredLR `intersect` filteredCR `intersect` filteredHR --`intersect` filteredCS
    --
    --           where filteredCC = filterCC correctCard oldGuess oldGameState
    --                     where filterCC :: Int -> [Card] -> GameState -> GameState
    --                           filterCC correctCard oldGuess oldGameState
    --                               | correctCard == 0        = filter (\xs -> length (intersect oldGuess xs) == 0) oldGameState
    --                               | otherwise               = filter (\xs -> length (intersect oldGuess xs) /= 0) oldGameState
    --                 filteredLR = filterLR lowerRank oldGuess oldGameState
    --                     where filterLR :: Int -> [Card] -> GameState -> GameState
    --                           filterLR lowerRank oldGuess oldGameState
    --                               | lowerRank == 0          = filter (\xs -> and (map (>= minimum [r | (Card s r) <- oldGuess]) [r1 | (Card s1 r1) <- xs])) oldGameState
    --                               | otherwise               = oldGameState
    --
    --                 filteredCR = filterCR correctRank oldGuess oldGameState
    --                     where filterCR :: Int -> [Card] -> GameState -> GameState
    --                           filterCR correctRank oldGuess oldGameState
    --                               | correctRank == 0        = filter (\xs -> length [(Card s1 r1) | (Card s1 r1) <- xs, (Card s2 r2) <- oldGuess, r1 == r2] == 0) oldGameState
    --                               | otherwise               = filter (\xs -> length [(Card s1 r1) | (Card s1 r1) <- xs, (Card s2 r2) <- oldGuess, r1 == r2] /= 0) oldGameState
    --
    --                 filteredHR = filterHR higherRank oldGuess oldGameState
    --                     where filterHR :: Int -> [Card] -> GameState -> GameState
    --                           filterHR higherRank oldGuess oldGameState
    --                               | higherRank == 0         = filter (\xs -> and (map (<= maximum [r | (Card s r) <- oldGuess]) [r1 | (Card s1 r1) <- xs])) oldGameState--filter () oldGameState
    --                               | otherwise               = oldGameState--filter () oldGameState
    --
    --
    --                 filteredCS = filterCS correctSuit oldGuess oldGameState
    --                     where filterCS :: Int -> [Card] -> GameState -> GameState
    --                           filterCS correctSuit oldGuess oldGameState
    --                               | correctSuit == 0        = filter (\xs -> length [(Card s1 r1) | (Card s1 r1) <- xs, (Card s2 r2) <- oldGuess, s1 == s2] == 0) oldGameState
    --                               | otherwise               = filter (\xs -> length [(Card s1 r1) | (Card s1 r1) <- xs, (Card s2 r2) <- oldGuess, s1 == s2] /= 0) oldGameState
