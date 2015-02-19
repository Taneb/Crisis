module Crisis.Strategy (chooseMove, setOutcome) where

import Control.Monad
import Data.Bits
import Data.Foldable (foldMap)
import Data.List (foldl')
import Data.Tuple
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V

import Crisis.Util

{-
TODO: make chooseMove and setOutcome work in parallel
      I believe this can be done with repa with accelerate as a future goal.
-}

oiot :: [Int]
oiot = [0..107]

coordToBit :: (Int, Int) -> Int
coordToBit (x, y) | y < 6 = x + 6 * y
                  | otherwise = x + 12 * y - 36

isInShipList :: Ship -> Int -> Bool
isInShipList ship shipList = testBit shipList (fromEnum ship)

chooseMove :: Board -> Vector (Board, Int) -> Board -> Maybe (Int, Int)
chooseMove misses universes emptycells =
  let finalScores = V.foldl' (\r n -> V.zipWith (+) r $ scoreUniverse n) (V.replicate 108 0) universes
      (bestscore, bests) =
        V.foldr (\(i, this) (score, bs) ->
                case this `compare` score of
                  LT -> (score, bs)
                  EQ -> (score, i : bs)
                  GT -> (this, [i])) (0, []) $ V.indexed finalScores
      coord = head bests -- TODO: random
  in if bestscore == 0 then Nothing
     else  Just $ swap $ if coord < 36
                         then coord        `divMod` 6
                         else (coord + 36) `divMod` 12
  where
    scoreUniverse (universe, remShips) = V.zipWith (+) scorePart1 scorePart2
      where
        -- handy synonym for the number of remaining ships in the universe
        noRemShips = popCount remShips

        -- adjust scores for cells the universe believes to be DEFINITELY
        -- occupied.
        scorePart1 = V.generate 108 $ \i ->
          if testBit emptycells i && testBit universe i
          then 6000 - 1000*noRemShips
          else 0

        -- adjust scores for cells the universe belieces to be POSSIBLY
        -- occupied, by considering where it is possible to place a ship that
        -- hasn't already been placed.
        --
        -- TODO: further vectorize, parrelellize?
        scorePart2 = foldl' (V.zipWith (+)) (V.replicate 108 0) $ do
          ship <- [Destroyer .. Carrier]
          guard $ ship `isInShipList` remShips
          placement <- V.toList $ allPositionsOf ship
          guard $ placement .&. (misses .|. complement emptycells .|. universe) == Board 0 0
          return $ V.generate 108 $ \i -> if testBit placement i then 1 + noRemShips else 0

setOutcome :: Board -> Vector (Board, Int) -> Board ->
              (Int, Int) -> Bool -> 
              (Board, Vector (Board, Int), Board)
setOutcome misses universes emptycells coord0 outcome =
  let coord = bit $  coordToBit coord0
      newemptycells = emptycells .&. complement coord
      newuniverses =
        if outcome
        then
          flip V.concatMap universes $ \(universe, remships) ->
          if coord .&. universe == Board 0 0
          then
            flip foldMap [i | i <- [Destroyer .. Carrier], i `isInShipList` remships] $ \ship ->
            flip V.map (V.filter (\placement ->
                                     placement .&. coord /= Board 0 0 &&
                                     placement .&. misses == Board 0 0 &&
                                     placement .&. universe == Board 0 0) $ allPositionsOf ship) $ \placement ->
            (universe .|. placement, remships - bit (fromEnum ship))
          else V.singleton (universe, remships)
        else
          V.filter (\(a, _) -> coord .&. a == Board 0 0) universes
  in (if outcome then misses else misses .|. coord, newuniverses, newemptycells)
