--Implement your solution here
--Remember to put function declarations as well
-- File    : Proj1.hs
-- Author  : Xingyu Chen <xingyuc5@student.unimelb.edu.au>
-- Purpose : An implementation of core guessing algorithms for a
--           card guessing game.

-- | We represent the guesser in a card guessing game. In order to
-- win the game, we should guess the same combination of cards
-- (answer) as chosen by answerer. To start off, we would choose a
-- reasonable initialGuess, and use the received feedback from
-- answerer to come out with our nextGuess. To make the guessing
-- efficient, we would keep the gameState in mind, which is the
-- remaining possible answers. The game would end when our guess
-- of cards is the same as the answer. We would aim to minimise
-- the number of guesses.


module Proj1 (feedback, initialGuess, nextGuess, GameState) where
import Card
import Data.List
import Data.Map (fromListWith, toList)

-- | All possible remaining answers of the game. An answer is a
-- list of 2 to 4 cards.
type GameState = [[Card]]


-- | feedback (+List answer, +List guess, -Tuple result)
-- Args:
--   answer: a list of cards represents the answer of game
--   guess: a list of cards represents the guess we make
-- Output：
--   result: a 5-tuple consists of results of comparison between
--           guess and answers.
-- |
feedback :: [Card] -> [Card] -> (Int, Int, Int, Int, Int)
feedback xs ys
    = (correctCard,lowerRank,correctRank,higherRank,correctSuit)
        where
            -- Compute the number of correct cards in guess
            correctCard = length (xs `intersect` ys)

            -- Compute the number of cards in answer with ranks
            -- lower than lowest rank of cards in guess
            lowerRank = length [(Card s1 r1) | (Card s1 r1) <- xs,
             let rMin = minimum [r2 | (Card s2 r2) <- ys],
              r1 < rMin]

            -- Compute the number of cards in answer with same
            -- rank as cards in guesses, one card is counted once
            correctRank = length (nubBy (\(x1, y1) (x2, y2)
             -> x1 == x2 || y1 == y2) [((Card s1 r1),
              (Card s2 r2)) | (Card s1 r1) <- xs,
               (Card s2 r2) <- ys, r1 == r2])

            -- Compute the number of cards in answer with ranks
            -- higher than the highest rank of cards in guess
            higherRank = length [(Card s1 r1) | (Card s1 r1) <- xs,
             let rMax = maximum [r2 | (Card s2 r2) <- ys],
              r1 > rMax]

            -- Compute the number of cards in anwer with same
            -- suit as cards in guesses, one card is counted once
            correctSuit = length (nubBy (\(x1, y1) (x2, y2)
             -> x1 == x2 || y1 == y2) [((Card s1 r1),
              (Card s2 r2)) | (Card s1 r1) <- xs,
               (Card s2 r2) <- ys, s1 == s2])


-- | initialGuess (+Int numCards, -Tuple (guess, gamestate))
-- Args:
--   numCards: an integer, the number of cards in the answer
--             chosen by answerer, indicates how many cards in our
--             guess
-- Output：
--   2-tuple of guess and gamestate:
--     guess: a list of cards, our initial guess of cards
--     gamestate: a list of list of cards, all possible answers
-- |
initialGuess :: Int -> ([Card], GameState)
initialGuess x
    = (guess, gameState)

          -- gameState is all possible answers
    where gameState = combinations x allCards

                    -- combinations function used to compute all
                    -- possible combinations of cards, i.e. answer
              where combinations :: Int -> [a] -> [[a]]
                    combinations 0 _ = [[]]
                    combinations r xs = [xs !! i : x | i <-
                     [0..(length xs)-1], x <- combinations
                      (r-1) (drop (i+1) xs)]

                    -- all enumerations of cards
                    allCards = [minBound .. maxBound :: Card]

          -- guess is the initialgeuss we choose by choosing
          -- cards with different suits and with ranks separated
          -- equally from each other and from top and bottom.
          guess = [Card s r | a <- [1..x],
           let s = suits !! ((a-1) `mod` suitCounts),
            let r = ranks !! (rankCounts `div` (x+1) * a)]
              where suits =  [Club ..]
                    ranks = [R2 ..]
                    suitCounts = 4
                    rankCounts = 13


-- | initialGuess (+tuple (List oldGuess, GameState oldGameState),
--                 +tuple results,
--                 -tuple (List newGuess, GameState newGamestate))
-- Args:
--   (oldGuess, oldGameState):
--   oldGuess: a list of cars, the previous guess we made
--   oldGameState: the previous remaining possible answers
--
--   results: 5-tuple caontains the five scores of feedback
-- Output:
--   (newGuess, newGameState):
--     newGameState: reduced remaining possible answers from
--                   oldGameState, only keep consistant possible
--                   answers.
--     newGuess: new guess chosen from the updated remaining
--               possible answers.
-- |
nextGuess :: ([Card], GameState) -> (Int, Int, Int, Int, Int) ->
    ([Card], GameState)
nextGuess ([], []) (0,0,0,0,0) = ([], [])
nextGuess (oldGuess, oldGameState) previousFeedback

      -- update gamestae by only keeping those consistant
    = let newGameState = [guess | guess <- oldGameState,
           feedback guess oldGuess == previousFeedback]

      -- use updated gamestate to find best guess
      in (findNextGuess newGameState, newGameState)
          where findNextGuess :: GameState -> [Card]
                findNextGuess [[]] = []
                findNextGuess gs
                    -- if game state is too large, just choose
                    -- first possible answer
                    | (length gs) > threshold        = head gs

                    -- otherwise, choose guess which would results
                    -- in fewest possible guesses
                    | otherwise = snd (minimum
                        ([(evaluate x gs, x) | x <- gs]))
                         where threshold = 2000

                               -- evaluate the sum of score of a guess
                               -- The score is computed by computing all
                               -- feedback of possible answers with a the guess
                               -- and group the feedbacks by their values,
                               -- then compute the sum of squares of sizes
                               evaluate :: [Card] -> GameState ->
                                Int
                               evaluate guessCandidate
                                possibleAnswers
                                   = sum (map (\(a,b) -> b^2)
                                    (toList (fromListWith (+)
                                     [(feedback answer
                                      guessCandidate, 1)| answer
                                       <- possibleAnswers])))
