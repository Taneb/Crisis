module Crisis.Strategy (chooseMove, setOutcome) where

import Control.Monad
import Control.Monad.ST
import Data.Array.IArray
import Data.Array.ST
import Data.Bits
import Data.Tuple

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

chooseMove :: Board -> [(Board, Int)] -> Board -> Maybe (Int, Int)
chooseMove misses universes emptycells =
  let finalScores = runSTUArray $ do
        r <- newArray (0,107) 0 :: ST s (STUArray s Int Int)
        forM_ universes $ scoreUniverse r
        return r
      (bestscore, bests) =
        foldr (\(i, this) (score, bs) ->
                case this `compare` score of
                  LT -> (score, bs)
                  EQ -> (score, i : bs)
                  GT -> (this, [i])) (0, []) $ assocs finalScores
      coord = head bests -- TODO: random
  in if bestscore == 0 then Nothing
     else  Just $ swap $ if coord < 36
                         then coord        `divMod` 6
                         else (coord + 36) `divMod` 12
  where
    -- might be a thought to not use ST at all?
    scoreUniverse finalScores (universe, remShips) = do
      scorePart1
      scorePart2
      where
        -- handy synonym for the number of remaining ships in the universe
        noRemShips = popCount remShips

        -- adjust scores for cells the universe believes to be DEFINITELY
        -- occupied.
        scorePart1 = forM_ [x | x <- oiot, testBit emptycells x] $ \i ->
          when (testBit universe i) $ do
            x <- readArray finalScores i
            writeArray finalScores i (x + 600 - 100*noRemShips)

        -- adjust scores for cells the universe belieces to be POSSIBLY
        -- occupied, by considering where it is possible to place a ship that
        -- hasn't already been placed.
        scorePart2 =
          forM_ [i|i<-[Destroyer .. Carrier],i`isInShipList`remShips] $ \ship ->
          forM_ (allPositionsOf ship) $ \placement ->
          when (placement.&.(misses.|.complement emptycells.|.universe) == Board 0 0) $
          forM_ oiot $ \i ->
          when (testBit placement i) $ do
            x <- readArray finalScores i
            writeArray finalScores i (x + 1 + noRemShips)

setOutcome :: Board -> [(Board, Int)] -> Board ->
              (Int, Int) -> Bool -> 
              (Board, [(Board, Int)], Board)
setOutcome misses universes emptycells coord0 outcome =
  let coord = bit $  coordToBit coord0
      newemptycells = emptycells .&. complement coord
      newuniverses =
        if outcome
        then do
          (universe, remships) <- universes
          if coord .&. universe == Board 0 0
            then do
            ship <- [i | i <- [Destroyer .. Carrier], i `isInShipList` remships]
            placement <- allPositionsOf ship
            guard $ placement .&. coord /= Board 0 0
            guard $ placement .&. misses == Board 0 0
            guard $ placement .&. universe == Board 0 0
            return (universe .|. placement, remships - bit (fromEnum ship))
            else return (universe, remships)
        else
          [(a, b) | (a, b) <- universes, coord .&. a == Board 0 0]

  in (if outcome then misses else misses .|. coord, newuniverses, newemptycells)
