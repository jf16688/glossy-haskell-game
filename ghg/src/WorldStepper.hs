module WorldStepper (worldStepper) where

import Lib
import Data.Maybe
import Data.List
import Debug.Trace

interval :: Float
interval = 0.5

merge :: (Ord a) => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
  | x > y = y:(merge (x:xs) ys)
  | otherwise = x:(merge xs (y:ys))

--Assumes both lists sorted
moveToForeground :: FallingBlock -> Fg -> Fg
moveToForeground fb@(FallingBlock tetra _ _) (Fg ts) =
  Fg $ merge (zip (blockPoints fb) (repeat tetra)) ts

fallBlock :: FallingBlock -> FallingBlock
fallBlock (FallingBlock tet (y, x) rot) = FallingBlock tet (y+1, x) rot

worldStepper :: Float -> Game -> Game
worldStepper dt (Menu menu rs) = Menu menu rs
worldStepper dt game
  | (accTime game) + dt < (interval / (acceleration game)) = game { accTime = (accTime game) + dt }
  | any (\((y,_), _) -> y == 0) (unFg . for $ game)= Menu (M 2) (rands game)
  | otherwise = game { for = for'''
                     , fall = chosenBlock
                     , accTime = 0
                     , rands   = rands' }
  where
    (Play for' opacity' mines' wtf' fall' _ (r:rands') accTime' _ _) = game

    fall'' = fallBlock fall'
    collided = hasCollided for' (fall'')

    chosenBlock :: FallingBlock
    chosenBlock = if collided
                    then newFallingBlock r -- Make new falling block
                    else fall''

    for'' = if collided
               then moveToForeground fall' for'
               else for'
    for''' = removeFullRows for''

    -- Minesweeper stuff

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

snd3 :: (a, b, c) -> b
snd3 (_, y, _) = y

-- This sucks, I'm so sorry
index :: [[(a, b)]] -> [[((Int, Int), a, b)]]
index xss = zipWith f [0..] (map (zip [0..]) xss)
  where
    f :: Int -> [(Int, (a, b))] -> [((Int, Int), a, b)]
    f y xts = map (\(x, (a, b)) -> ((y, x), a, b)) xts


-- getMines :: Background -> [(Int, Int)]
-- getMines (Background bss) = map fst3 (concat (map (filter isMine) (index bss)))
--   where
--     isMine :: ((Int, Int), Maybe Char, Int) -> Bool
--     isMine t = isJust (snd3 t)

touchingMine :: Mines -> FallingBlock -> Bool
touchingMine (Mines ms) fb = any ((flip elem) (map fst ms)) (blockPoints fb)



removeFullRows :: Fg -> Fg
removeFullRows (Fg xs) = Fg (fst . foldr f' ([],0) $ ys)  where
  ys = groupBy (f (==)) . sortBy (f compare) $ xs
  f :: (Int -> Int -> b) -> ((Int,Int),Tetramino) -> ((Int,Int),Tetramino) -> b
  f g ((a,_),_) ((b,_),_) = g a b
  f' :: [((Int,Int),Tetramino)] -> ([((Int,Int),Tetramino)], Int) -> ([((Int,Int),Tetramino)], Int)
  f' ps (ts, offset)
    | length ps /= worldWidth = (map (\((y, x), t) -> ((y + offset, x), t)) ps ++ ts, offset)
    | otherwise = (ts, offset + 1)
