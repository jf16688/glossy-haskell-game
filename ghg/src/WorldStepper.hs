{-# LANGUAGE RecordWildCards #-}

module WorldStepper (worldStepper) where

import LibGame
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
worldStepper dt game@Play{..}
  | accTime + dt < (interval / acceleration) = game { accTime = accTime + dt }
  | any (\((y,_), _) -> y == 0) (unFg for) = Menu (GameOver foundChars (unWord2Find wtf) "") rands
  | otherwise = encircled (game { for = for''
                                , fall = chosenBlock
                                , opacity = opacity'
                                , accTime = 0
                                , rands   = rands' })
  where
    -- (Play for' opacity' mines' wtf' fall' _ (r:rands') accTime' _) = game
    -- Pattern matches on everything
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
encircled gameCurr@(Play {mines = m, for = forValue, foundChars = cC}) = gameCurr {mines = (Mines vM),foundChars = cC ++ vC, for = vF} where
  (vM,vC,vF) = adjustBasedOnBool v1 (unMines m)

  adjustBasedOnBool :: [Bool] -> [((Int,Int),Char)] -> ([((Int,Int),Char)],[Char],Fg)
  adjustBasedOnBool [] xs = (xs,[],forValue)
  adjustBasedOnBool xs [] = ([],[],forValue)
  adjustBasedOnBool (True:bs)  (((y,x),c):xs) = threaderMine ((y,x),c) (adjustBasedOnBool bs xs)
  adjustBasedOnBool (False:bs) (((y,x),c):xs) = threaderChar (c) ((y,x),I) (adjustBasedOnBool bs xs)

  threaderChar :: Char -> ((Int,Int),Tetramino) -> ([((Int,Int),Char)],[Char],Fg) -> ([((Int,Int),Char)],[Char],Fg)
  threaderChar ks xs (bs,cs,(Fg fg)) = (bs,ks:cs,(Fg (xs:fg)))

  explode :: Int -> [(Int, Int)] -> [((Int,Int),Tetramino)] -> [(Int,Int)] -> ([((Int,Int),Tetramino)])
  explode 0 fs gs exs = (remove exs gs)
  explode _ [] gs exs = (remove exs gs)
  explode n fs gs exs = explode (n - 1) (fmap fst fs') gs (exs ++ exs') where
    (fs',exs') = (remove' fs) . (\(a) -> (a,[]) ) . remdups . expand $ (fmap ((\(a,b) -> ((a,b),Nothing)) . fst) gs)

  threaderMine :: ((Int,Int),Char) -> ([((Int,Int),Char)],[Char],Fg) -> ([((Int,Int),Char)],[Char],Fg)
  threaderMine a (as,bs,fg) = (a:as,bs,fg)

  v1 = fmap (testWith 30 (((fmap fst) (unFg forValue)))) (fmap ((\x -> ([(x,Nothing)])) . fst) (unMines m))

  testWith :: Int -> [(Int,Int)] -> ([((Int, Int),Maybe Rotation)]) -> Bool
  --testWith [] gs = False
  testWith 0 fs gs = False
  testWith _ fs [] = False
  testWith n fs gs = (hasTermed v2) || testWith (n - 1) fs v2 where
    v2 = remove fs . remdups . expand $ gs

  remdups :: [((Int, Int),Maybe Rotation)] -> [((Int, Int),Maybe Rotation)]
  remdups = nubBy (\(a, _) (b, _) -> a == b)

  -- False if edge has been hit
  hasTermed :: [((Int, Int),Maybe Rotation)] -> Bool
  hasTermed = (any testEdge)

  -- True if edge is hit
  testEdge :: ((Int,Int),Maybe Rotation) -> Bool
  testEdge ((y,x),_) = y < 0 || x < 0 || y >= worldHeight || x >= worldWidth

  expand :: [((Int, Int),Maybe Rotation)] -> [((Int, Int),Maybe Rotation)]
  expand xs = xs >>= expand'

  expand' :: ((Int, Int),Maybe Rotation) -> [((Int, Int),Maybe Rotation)]
  expand' ((y,x),Nothing) = [((y-1,x),Just North),((y,x-1),Just East),((y+1,x),Just South),((y,x+1),Just West)]
  expand' ((y,x),(Just North)) = [((y-1,x),Just North),((y,x-1),Just East),((y,x+1),Just West)]
  expand' ((y,x),(Just South)) = [((y,x-1),Just East),((y+1,x),Just South),((y,x+1),Just West)]
  expand' ((y,x),(Just East)) = [((y-1,x),Just North),((y,x-1),Just East),((y+1,x),Just South)]
  expand' ((y,x),(Just West)) = [((y-1,x),Just North),((y+1,x),Just South),((y,x+1),Just West)]

  -- remove ==
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

remove :: Eq a => [a] -> ([(a,b)]) -> ([(a,b)])
remove [] xs = xs
remove (y:ys) xs = remove ys (removeValue y xs)

removeValue :: Eq a => a -> ([(a,b)]) -> ([(a,b)])
removeValue _ [] = []
removeValue a ((b,dir):xs) | a == b = removeValue a xs
                                        | otherwise = (b,dir) :(removeValue a xs)

remove' :: Eq a => [a] -> ([(a,b)],[a]) -> ([(a,b)],[a])
remove' [] xs = xs
remove' (y:ys) xs = remove' ys (removeValue' y xs)

removeValue' :: Eq a => a -> ([(a,b)],[a]) -> ([(a,b)],[a])
removeValue' _ ([],ys) = ([],ys)
removeValue' a (((b,dir):xs),ys) | a == b    = (removeValue' a (xs,a:ys))
                                 | otherwise = threadThrough (b,dir) (removeValue' a (xs,ys)) where
                          threadThrough :: (a,b) -> ([(a,b)],[a]) -> ([(a,b)],[a])
                          threadThrough (b,dir) (xs,as) = ((b,dir):xs,as)
