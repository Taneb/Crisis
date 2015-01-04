module Main where

import Control.Monad
import Control.Monad.ST
import Data.Array.IArray
import Data.Array.ST
import Data.Bits
import Data.List
import Data.Tuple
import System.Random

import Crisis.Util

oiot :: [Int]
oiot = [0..107]

weightAnswer :: Int -> Maybe Int -> Int
weightAnswer _ _  = 1

isValidCoord :: (Int, Int) -> Bool
isValidCoord (x, y) =
  x >= 0 && y >= 0 && y < 12 && x < if y < 6 then 6 else 12

allCoords :: [(Int, Int)]
allCoords = [(x, y) | y <- [0..11], x <- [0..if y < 6 then 5 else 11]]

coordToBit :: (Int, Int) -> Int
coordToBit (x, y) | y < 6 = x + 6 * y
                  | otherwise = x + 12 * y - 36

boardToJaggedArray :: Integer -> [[Bool]]
boardToJaggedArray board =
  [[testBit board bitPos |
    x <- [0..if y < 6 then 5 else 11],
    let bitPos = coordToBit (x, y)]
  | y <- [0..11]]

initialShipList :: Int
initialShipList = 1 `shiftL` 5 - 1

isInShipList :: Ship -> Int -> Bool
isInShipList ship shipList = testBit shipList (fromEnum ship)

showBoard :: Integer -> String
showBoard board = unlines $ do
  y <- [-1..12]
  return $ do
    x <- [-1..if y < 5 then 6 else 12]
    return $ case () of
      _ | y == -1 && x == -1                                           -> '┌'
        | y == -1 && x ==  6 || y ==  5 && x == 12                     -> '┐'
        | y == 12 && x == 12                                           -> '┘'
        | y == 12 && x == -1 || y ==  5 && x ==  6                     -> '└'
        |            x == -1 || y  <  5 && x ==  6 || x == 12          -> '│'
        | y == -1            || y == 12            || y == 5 && x  > 6 -> '─'
        | otherwise -> if testBit board (coordToBit (x, y))
                       then '#'
                       else ' '

chooseMove :: Integer -> [(Integer, Int)] -> Integer -> Maybe (Int, Int)
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
            writeArray finalScores i (x + weightAnswer noRemShips Nothing)

        -- adjust scores for cells the universe belieces to be POSSIBLY
        -- occupied, by considering where it is possible to place a ship that
        -- hasn't already been placed.
        scorePart2 =
          forM_ [i|i<-[Destroyer .. Carrier],i`isInShipList`remShips] $ \ship ->
          forM_ (allPositionsOf ship) $ \placement ->
          when (placement.&.(misses.|.complement emptycells.|.universe) == 0) $
          forM_ oiot $ \i ->
          when (testBit placement i) $ do
            x <- readArray finalScores i
            writeArray finalScores i (x + weightAnswer noRemShips (Just 1))

setOutcome :: Integer -> [(Integer, Int)] -> Integer ->
              (Int, Int) -> Bool -> 
              (Integer, [(Integer, Int)], Integer)
setOutcome misses universes emptycells coord0 outcome =
  let coord = 1 `shiftL` coordToBit coord0
      newemptycells = emptycells .&. complement coord
      newuniverses =
        if outcome
        then do
          (universe, remships) <- universes
          if coord .&. universe == 0
            then do
            ship <- [i | i <- [Destroyer .. Carrier], i `isInShipList` remships]
            placement <- allPositionsOf ship
            guard $ placement .&. coord /= 0
            guard $ placement .&. misses == 0
            guard $ placement .&. universe == 0
            return (universe .|. placement, remships - 1 `shiftL` fromEnum ship)
            else return (universe, remships)
        else
          [(a, b) | (a, b) <- universes, coord .&. a == 0]
  in (if outcome then misses else misses .|. coord, newuniverses, newemptycells)

genBoard :: RandomGen g => g -> (Integer, g)
genBoard g0 = head $ do
  let (as, g1) = shuffle (allPositionsOf Carrier) g0
  a <- as
  let ra = a
  let (hs, g2) = shuffle (allPositionsOf Hovercraft) g1
  h <- hs
  guard (ra.&.h == 0)
  let rh = ra .|. h
  let (bs, g3) = shuffle (allPositionsOf Battleship) g2
  b <- bs
  guard (rh.&.b == 0)
  let rb = rh .|. b
  let (cs, g4) = shuffle (allPositionsOf Cruiser) g3
  c <- cs
  guard (rb.&.c == 0)
  let rc = rb .|. c
  let (ds, g5) = shuffle (allPositionsOf Destroyer) g4
  d <- ds
  guard (rc.&.d == 0)
  let rd = rc .|. d
  return (rd, g5)
  where
    shuffle :: RandomGen g => [a] -> g -> ([a], g)
    shuffle as g =
      let (g1, g2) = split g
          l = length as
      in (map (as !!) $ take l $ nub $ randomRs (0, l - 1) g1, g2)

uncurry3 :: (t1 -> t2 -> t3 -> t) -> (t1, t2, t3) -> t
uncurry3 f (x,y,z) = f x y z

re :: Integer -> (Integer, [(Integer, Int)], Integer) -> Int -> IO Int
re board c@(ms,_,es) n = do
  putStrLn $ showBoard $ complement es
  putStrLn $ showBoard $ complement es .&. complement ms
  let m0 = uncurry3 chooseMove c
  case m0 of
    Nothing -> return n
    Just m -> do
      print m
      let r = testBit board (coordToBit m)
      putStrLn $ if r then "Hit" else "Miss"
      re board (uncurry3 setOutcome c m r) (n + 1)

main :: IO ()
main = do
  r <- newStdGen
  print r
  let (b, _) = genBoard r
  re b (0, [(0,initialShipList)], 1 `shiftL` 108 - 1)  0 >>=
    print
  
