{-# LANGUAGE RecordWildCards #-}

module WorldStepper (worldStepper) where

import Lib
import Data.Maybe
import Data.List
import Debug.Trace
import qualified Data.Map.Lazy as M

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
  | accTime + dt < (interval / acceleration) = game { accTime = accTime + dt }
  | any (\((y,_), _) -> y == 0) (unFg for) = Menu (M 2) rands
  | otherwise = encircled (game { for = for''
                                , fall = chosenBlock
                                , opacity = opacity'
                                , accTime = 0
                                , rands   = rands' })
  where
    -- (Play for' opacity' mines' wtf' fall' _ (r:rands') accTime' _) = game
    -- Pattern matches on everything
    Play{..} = game
    (r:rands') = rands

    fall' = fallBlock fall
    collided = hasCollided for (fall')

    chosenBlock :: FallingBlock
    chosenBlock = if collided
                    then newFallingBlock r -- Make new falling block
                    else fall'

    for' = if collided
               then moveToForeground fall for
               else for
    for'' = removeFullRows for'

    -- Minesweeper stuff
    opacity' :: Opacity
    opacity' = if touchingMine mines chosenBlock
                  then incOpacity opacity chosenBlock
                  else decOpacity opacity

touchingMine :: Mines -> FallingBlock -> Bool
touchingMine (Mines ms) fb = any ((flip elem) (map fst ms)) (blockPoints fb)

incOpacity :: Opacity -> FallingBlock -> Opacity
incOpacity op fb = foldr f op (blockPoints fb)
  where
    f :: (Int, Int) -> Opacity -> Opacity
    f pos (Opacity m)
      | M.member pos m = Opacity (M.adjust (+30) pos m)
      | otherwise      = Opacity (M.insert pos 30 m)

decOpacity :: Opacity -> Opacity
decOpacity (Opacity m) = Opacity (M.fromList (filter ((0 <) . snd) (M.toList (fmap (\x -> x - 1) m))))
-- decOpacity (Opacity m) = Opacity (M.fromList (filter ((0 <) . snd) (M.toList (fmap ((-)1) m))))

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

snd3 :: (a, b, c) -> b
snd3 (_, y, _) = y


index :: [[a]] -> [[((Int, Int), a)]]
index xss = zipWith f [0..] (map (zip [0..]) xss)
  where
    f :: Int -> [(Int, a)] -> [((Int, Int), a)]
    f y xts = map (\(x, t) -> ((y, x), t)) xts

-- getMines :: Background -> [(Int, Int)]
-- getMines (Background bss) = map fst3 (concat (map (filter isMine) (index bss)))
--   where
--     isMine :: ((Int, Int), Maybe Char, Int) -> Bool
--     isMine t = isJust (snd3 t)


encircled :: Game -> Game
-- encircled x = x
encircled gameCurr@(Play {mines = m, for = forValue, foundChars = cC}) = gameCurr {mines = (Mines vM),foundChars = cC ++ vC} where
  (vM,vC) = adjustBasedOnBool v1 (unMines m)

  adjustBasedOnBool :: [Bool] -> [((Int,Int),Char)] -> ([((Int,Int),Char)],[Char])
  adjustBasedOnBool [] xs = (xs,[])
  adjustBasedOnBool xs [] = ([],[])
  adjustBasedOnBool (True:bs)  (((y,x),c):xs) = threaderMine ((y,x),c) (adjustBasedOnBool bs xs)
  adjustBasedOnBool (False:bs) (((y,x),c):xs) = threaderChar (c) (adjustBasedOnBool bs xs)

  threaderChar :: Char -> ([((Int,Int),Char)],[Char]) -> ([((Int,Int),Char)],[Char])
  threaderChar ks (bs,cs) = (bs,ks:cs)

  threaderMine :: ((Int,Int),Char) -> ([((Int,Int),Char)],[Char]) -> ([((Int,Int),Char)],[Char])
  threaderMine a (as,bs) = (a:as,bs)

  v1 = fmap (testWith 20 (((fmap fst) (unFg forValue)))) (fmap ((\x -> ([(x,Nothing)])) . fst) (unMines m))

  testWith :: Int -> [(Int,Int)] -> ([((Int, Int),Maybe Rotation)]) -> Bool
  --testWith [] gs = False
  testWith 0 fs gs = False
  testWith _ fs [] = False
  testWith n fs gs = (hasTermed v2) || testWith (n - 1) fs v2 where
    v2 = (remove fs ((remdups . expand) (remove fs gs)))

  remdups :: [((Int, Int),Maybe Rotation)] -> [((Int, Int),Maybe Rotation)]
  remdups = (fmap head) . (groupBy g) . (sortBy f)
  f :: ((Int, Int),Maybe Rotation) -> ((Int, Int),Maybe Rotation) -> Ordering
  f ((a,_),_) ((b,_),_) = compare a b
  g :: ((Int, Int),Maybe Rotation) -> ((Int, Int),Maybe Rotation) -> Bool
  g ((y1,x1),_) ((y2,x2),_) = (y2 == y1) && (x2 == x1)

  -- False if edge has been hit
  hasTermed :: [((Int, Int),Maybe Rotation)] -> Bool
  hasTermed = (any testEdge)

  -- True if edge is hit
  testEdge :: ((Int,Int),Maybe Rotation) -> Bool
  testEdge ((y,x),_) = y < 0 || x < 0 || y >= worldHeight || x >= worldWidth

  expand :: [((Int, Int),Maybe Rotation)] -> [((Int, Int),Maybe Rotation)]
  expand [] = []
  expand (((y,x),Nothing):xs) = [((y-1,x),Just North),((y,x-1),Just East),((y+1,x),Just South),((y,x+1),Just West)] ++ (expand xs)
  expand (((y,x),(Just North)):xs) = [((y-1,x),Just North),((y,x-1),Just East),((y,x+1),Just West)] ++ (expand xs)
  expand (((y,x),(Just South)):xs) = [((y,x-1),Just East),((y+1,x),Just South),((y,x+1),Just West)] ++ (expand xs)
  expand (((y,x),(Just East)):xs) = [((y-1,x),Just North),((y,x-1),Just East),((y+1,x),Just South)] ++ (expand xs)
  expand (((y,x),(Just West)):xs) = [((y-1,x),Just North),((y+1,x),Just South),((y,x+1),Just West)] ++ (expand xs)

  remove :: [(Int,Int)] -> ([((Int, Int),Maybe Rotation)]) -> ([((Int, Int),Maybe Rotation)])
  remove [] xs = xs
  remove (y:ys) xs = remove ys (removeValue y xs)

  removeValue :: (Int,Int) -> ([((Int, Int),Maybe Rotation)]) -> ([((Int, Int),Maybe Rotation)])
  removeValue _ [] = []
  removeValue (y1,x1) (((y2,x2),dir):xs) | y1 == y2 && x1 == x2 = removeValue (y1,x1) xs
                                         | otherwise            = ((y2,x2),dir) : (removeValue (y1,x1) xs)
encircled (Menu a b) = Menu a b


removeFullRows :: Fg -> Fg
removeFullRows (Fg xs) = Fg (fst . foldr f' ([],0) $ ys)  where
  ys = groupBy (f (==)) . sortBy (f compare) $ xs
  f :: (Int -> Int -> b) -> ((Int,Int),Tetramino) -> ((Int,Int),Tetramino) -> b
  f g ((a,_),_) ((b,_),_) = g a b
  f' :: [((Int,Int),Tetramino)] -> ([((Int,Int),Tetramino)], Int) -> ([((Int,Int),Tetramino)], Int)
  f' ps (ts, offset)
    | length ps /= worldWidth = (map (\((y, x), t) -> ((y + offset, x), t)) ps ++ ts, offset)
    | otherwise = (ts, offset + 1)
