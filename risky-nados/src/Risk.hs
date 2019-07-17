{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random

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

data Death = Attacker | Defender
  deriving (Eq)

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }

-- assuming Army to be minimum 2
-- leave 1 behind and return 2 or less
defend :: Army -> Int
defend n = min 2 (n - 1)

attack :: Army -> Int
attack n = min 3 (n - 1)

kill :: (DieValue, DieValue) -> Death
kill (ad, dd)
  | unDV ad > unDV dd = Defender
  | otherwise         = Attacker

battle :: Battlefield -> Rand StdGen Battlefield
battle (Battlefield as ds) =
  let an  = attack as -- compute number of units
      dn  = defend ds
      ads = sequence . replicate an $ die -- roll for each unit
      dds = sequence . replicate dn $ die
      adp = zip <$> ads <*> dds -- pair the rolls
  in do
    deaths <- map kill <$> adp
    return (Battlefield {
      attackers = as - (length . filter (== Attacker) $ deaths),
      defenders = ds - (length . filter (== Defender) $ deaths) })

invade :: Battlefield -> Rand StdGen Battlefield
invade bf@(Battlefield as ds)
  | as < 2 || ds == 0 = return bf
  | otherwise         = battle bf >>= invade

attackWinning :: Battlefield -> Bool
attackWinning b = attackers b > defenders b

attackWinRatio :: [Bool] -> Double
attackWinRatio l = (fromIntegral (length . filter id $ l)) / (fromIntegral (length l))

successProb :: Battlefield -> Rand StdGen Double
successProb bs = do
  res <- sequence . map invade . replicate 1000 $ bs
  return (attackWinRatio . map attackWinning $ res)
