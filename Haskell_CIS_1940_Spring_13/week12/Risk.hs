{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Data.List (sortOn)
import Data.Ord (Down(..))

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int }
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }
  deriving (Show)

rollNTimes :: Int -> Rand StdGen [DieValue]
rollNTimes 0 = return []
rollNTimes n = do
  i <- die
  is <- rollNTimes (n-1)
  return (i : is)

battleOne :: Battlefield -> (DieValue, DieValue)  -> Battlefield
battleOne bf (x,y)
  | unDV x > unDV y = Battlefield (attackers bf) (defenders bf - 1)
  | otherwise = Battlefield (attackers bf - 1) (defenders bf)


battle :: Battlefield -> Rand StdGen Battlefield
battle bf = do
  atcks <- rollNTimes $ min 3 $ attackers bf
  defs <- rollNTimes $ min 2 $ defenders bf
  let newBf = foldl battleOne bf (zip (sortOn Down atcks) (sortOn Down defs))
  return newBf


-- die >>= (\dieVal -> )


-- Exercise 3

isDefenderZero :: Battlefield -> Bool
isDefenderZero bf = defenders bf <= 0 || attackers bf < 2

invade :: Battlefield -> Rand StdGen Battlefield
invade bf = bfGen >>= (\bf' ->
                        if isDefenderZero bf'
                        then return bf'
                        else invade bf'
                      )
            where bfGen = battle bf


-- Exercise 4
successProb :: Battlefield -> Rand StdGen Double
successProb bf = do
  rounds <- roundsM
  let prob = sum (map (\bf' -> if defenders bf' <=0 then 1 else 0) rounds) / 1000
  return prob
  where
    invaded = invade bf
    roundsM = mapM (\_ -> invaded) [1..1000]


-- testing
testBf = Battlefield 10 10
