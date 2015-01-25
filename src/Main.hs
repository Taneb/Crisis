module Main (main) where

import Control.Monad
import Data.Bits
import Data.List
import Data.Vector (Vector)
import qualified Data.Vector as V
import System.Console.ANSI
import System.Random

import Crisis.Strategy
import Crisis.Util

coordToBit :: (Int, Int) -> Int
coordToBit (x, y) | y < 6 = x + 6 * y
                  | otherwise = x + 12 * y - 36

initialShipList :: Int
initialShipList = bit 5 - 1

showBoard :: Board -> Board -> String
showBoard misses hits = unlines $ do
  y <- [-1..12]
  return $ do
    x <- [-1..if y < 5 then 6 else 12]
    case () of
      _ | y == -1 && x == -1                                           -> "┌"
        | y == -1 && x ==  6 || y ==  5 && x == 12                     -> "┐"
        | y == 12 && x == 12                                           -> "┘"
        | y == 12 && x == -1 || y ==  5 && x ==  6                     -> "└"
        |            x == -1 || y  <  5 && x ==  6 || x == 12          -> "│"
        | y == -1            || y == 12            || y == 5 && x  > 6 -> "─"
        | testBit misses (coordToBit (x, y)) -> setSGRCode [SetColor Foreground Vivid Blue] ++ "~" ++ setSGRCode [Reset]
        | testBit hits (coordToBit (x, y)) -> setSGRCode [SetColor Foreground Vivid Red] ++ "O" ++ setSGRCode [Reset]
        | otherwise -> " "

genBoard :: RandomGen g => g -> (Board, g)
genBoard g0 = head $ do
  let (ds, g1) = genShip Destroyer (Board 0 0) g0
  d <- ds
  let (cs, g2) = genShip Cruiser d g1
  c <- cs
  let (bs, g3) = genShip Battleship c g2
  b <- bs
  let (hs, g4) = genShip Hovercraft b g3
  h <- hs
  let (as, g5) = genShip Carrier h g4
  a <- as
  return (a, g5)
  where
    genShip :: RandomGen g => Ship -> Board -> g -> ([Board], g)
    genShip s b ga = let (s', gb) = shuffle (V.toList $ allPositionsOf s) ga
                     in flip (,) gb $ do
                       sp <- s'
                       guard (sp .&. b == Board 0 0)
                       let r = b .|. sp
                       return r
    shuffle :: RandomGen g => [a] -> g -> ([a], g)
    shuffle as g =
      let (g1, g2) = split g
          l = length as
      in (map (as !!) $ take l $ nub $ randomRs (0, l - 1) g1, g2)

uncurry3 :: (t1 -> t2 -> t3 -> t) -> (t1, t2, t3) -> t
uncurry3 f (x,y,z) = f x y z

re :: Board -> (Board, Vector (Board, Int), Board) -> Int -> IO Int
re board c@(x,_,y) n = do
  putStrLn $ showBoard x (complement y .&. complement x)
  print n
  let m0 = uncurry3 chooseMove c
  case m0 of
    Nothing -> return n
    Just m -> do
      let r = testBit board (coordToBit m)
      re board (uncurry3 setOutcome c m r) (n + 1)

sample :: Int -> IO (Double, Double)
sample size = do
  rs <- go size
  let m = fromIntegral (sum rs) / fromIntegral size
  let s = sqrt $ sum [x*x | r <- rs, let x = fromIntegral r - m]
               / fromIntegral size
  return (m, s)
  where
    go 0 = return []
    go n = do
      print n
      r <- newStdGen
      let (b, _) = genBoard r
      x <- re b (Board 0 0, V.singleton (Board 0 0,initialShipList), Board 18446744073709551615 17592186044415) 0
      fmap (x:) (go (n - 1))

main :: IO ()
main = do
  res <- sample 1
  print res
