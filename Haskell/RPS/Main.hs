{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.List
import           Data.Map.Strict
import           Data.Maybe
import           Data.Random.Extras
import           Data.Random.Source.DevRandom
import           Data.RVar

data Hand = Rock | Scissors | Paper deriving (Ord, Eq, Show)

possibleHands :: [Hand]
possibleHands = [Rock, Paper, Scissors]

randomHand :: IO Hand
randomHand = runRVar handChoice DevRandom
    where
        handChoice = choice possibleHands
-- maybe remove io here?
performPlay :: Hand -> Hand -> IO ()
performPlay userHand compHand = do
    print $ "You've played: " ++ show userHand
    print $ "Program played: " ++ show compHand
    print $ playAgainst userHand compHand

playAgainst :: Hand -> Hand -> String
playAgainst userHand compHand
    | playAgainstInt userHand compHand == 1 = "You have won"
    | playAgainstInt userHand compHand == 0 = "Draw!"
    | otherwise = "You have lost!"
    where
        playAgainstInt :: Hand -> Hand -> Int
        playAgainstInt hand otherHand
            | hand == otherHand = 0
        playAgainstInt Rock otherHand
            | otherHand == Paper = -1
            | otherHand == Scissors = 1
        playAgainstInt Paper otherHand
            | otherHand == Scissors = -1
            | otherHand == Rock = 1
        playAgainstInt Scissors otherHand
            | otherHand == Rock = -1
            | otherHand == Paper = 1
        playAgainstInt _ _ = error "non-existing combination"

deserializeHand :: Char -> Hand
deserializeHand inputChar
    | inputChar == 'r' = Rock
    | inputChar == 's' = Scissors
    | inputChar == 'p' = Paper
    | otherwise = error $ "Non-existing serialized Hand passed: " ++ [inputChar]

serializeHand :: Hand -> Char
serializeHand inputHand
    | inputHand == Rock = 'r'
    | inputHand == Scissors = 's'
    | inputHand == Paper = 'p'
    | otherwise = error "non-existing hand"

getDatabaseContent :: IO String
getDatabaseContent = readFile "db"

writeHandToDB :: Hand -> IO ()
writeHandToDB h = writeCharToDefaultDB $ serializeHand h
    where
        writeCharToDefaultDB :: Char -> IO ()
        writeCharToDefaultDB c = appendFile "db" [c]

-- it could be a simple list instead of maybes.
type Cache = [Maybe Char]
updateLastThreePlays :: Cache -> Hand -> Cache
updateLastThreePlays (c0 : c1 : [_]) nc = (Just $ serializeHand nc) : c0 : [c1]
updateLastThreePlays _ _ = [Nothing, Nothing, Nothing] -- empty cache

getBeatingHand :: Hand -> Hand
getBeatingHand inHand
    | inHand == Rock = Paper
    | inHand == Paper = Scissors
    | inHand == Scissors = Rock
    | otherwise = undefined

-- type alias or data type?
getHandOccurencesBasedOnCache :: String -> String -> Map Hand Int

getHandOccurencesBasedOnCache cache content =
    go cache content empty
    where
        go :: String -> String -> Map Hand Int -> Map Hand Int
        go key content accMap
            | contentLength < 2 || keyLength >= contentLength = accMap
            | reverse key `isPrefixOf` content = go key (tail content) (insertWith (+) (deserializeHand (content !! keyLength)) 1 accMap)
            | otherwise = go key (tail content) accMap
            where
                keyLength = length key
                contentLength = length content

-- remove io here (?)
chooseBestHand :: Map Hand Int -> IO Hand
chooseBestHand nextHandOccurences =
    go Nothing $ toList nextHandOccurences
    where
        go :: Maybe (Hand, Int) -> [(Hand, Int)] -> IO Hand
        go _ [] = randomHand
        go Nothing [(h, _)] = return $ getBeatingHand h
        go (Just (h0, n0)) [(h1, n1)]
            | n0 > n1 = go Nothing [(h0, n0)]
            | n0 < n1 = go Nothing [(h1, n1)]
            | otherwise = do
                h <- selectRandomHandFromEqOccurences h0 h1
                go Nothing [(h, n0)]
        go (Just (h0, n0)) ((h1, n1) : cns)
            | n0 > n1 = go (Just (h0, n0)) cns
            | n0 < n1 = go (Just (h1, n1)) cns
            | otherwise =  do
                h <- selectRandomHandFromEqOccurences h0 h1
                go (Just (h, n0)) cns
        go Nothing ((h, n) : cns) = go (Just (h, n)) cns
        selectRandomHandFromEqOccurences :: Hand -> Hand -> IO Hand
        selectRandomHandFromEqOccurences h1 h2 = runRVar (choice [h1, h2]) DevRandom

calculateAIHand :: Cache -> String -> IO Hand
calculateAIHand cs dbContent =
    chooseBestHand $ getHandOccurencesBasedOnCache cacheValue dbContent
        where
        cacheValue = catMaybes cs

-- validation missing
getAndValidateUserHand :: IO Hand
getAndValidateUserHand = do
    putStrLn "Choose (r)ock, (p)aper or (s)cissors"
    userChar <- getLine
    writeHandToDB $ deserializeHand $ head userChar
    return $ deserializeHand $ head userChar

playRound :: Cache -> IO ()
playRound cache = do
    content <- getDatabaseContent
    aiHand <- calculateAIHand cache content
    userHand <- getAndValidateUserHand
    userHand `performPlay` aiHand
    playRound $ updateLastThreePlays cache userHand

main :: IO ()
main = playRound [Nothing, Nothing, Nothing]
