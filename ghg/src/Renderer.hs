module Renderer (game2Pic) where

import Lib

blockSize :: Float
blockSize = 40

basicBlock :: Float -> Float -> Picture
basicBlock x y = (Polygon [(0,0),(0,y),(x,y),(x,0)])

game2Pic :: Game -> Picture
game2Pic (Menu x _) = rendMenu x
game2Pic (Play {for = f, back = b, fall = p, word = w}) = Translate (-xLength/2) (-yLength/2) (Pictures (bg ++ rendBack b ++ rendFor f yLength ++ rendFall p yLength ++ (fmap (Translate (xLength) (0)) (rendWord w)))) where
  bArr    = g b
  yLength = (fromIntegral (length bArr)) * blockSize
  xLength = (fromIntegral (length (head bArr))) * blockSize
  bg :: [Picture]
  bg      = [Color (makeColor 0.95 0.95 0.95 1) (basicBlock xLength yLength)]
  g :: Background -> [[(Maybe Char, Int)]]
  g (Background bs) = bs

rendMenu :: Menu -> Picture
rendMenu (M i) = Pictures [Color (red) (Polygon [(-170,-50),(170,-50),(170,50),(-170,50)]),
                              Translate (-140) (-25) (Scale 0.4 0.4  (Text s)),
                              Color (blue) (Line [(-170,-50),(170,-50),(170,50),(-170,50),(-170,-50)])
                             ]
  where
    s = case i of
      1 -> "Press Space to Play Game"
      2 -> "Game Over: Try Again? (Press space noob)"
      3 -> "YOU WIN!!!!"
      _ -> "Something went wrong"

rendBack :: Background -> [Picture]
rendBack (Background ps) = arrayBasic (fmap (fmap f) ps) where
  f :: (Maybe Char, Int) -> Picture
  f (_, x) = Color (makeColor 0 0 0 (1 - g x)) (basicBlock blockSize blockSize)
  g :: Int -> Float
  g x = exp (-(((fromIntegral x))/ 3))

rendFor :: Fg -> Float -> [Picture]
rendFor (Fg ps) s = (fmap (f s)) ps where
  f :: Float -> ((Int,Int),Tetramino) -> Picture
  f s ((y,x) ,t) = rendTetramino t (Translate ((fromIntegral x) * blockSize) (s - (fromIntegral y) * blockSize) (basicBlock blockSize blockSize))

rendFall :: FallingBlock -> Float -> [Picture]
rendFall b@(FallingBlock tet _ _) t = ((fmap ((rendTetramino tet) . (f t))) . blockPoints) b where
  f :: Float -> (Int,Int) -> Picture
  f top (y,x) = Translate ((fromIntegral x) * blockSize) (top - ((fromIntegral y) * blockSize)) (basicBlock blockSize blockSize)

rendWord :: String -> [Picture]
rendWord s = [Text s]

--makeColor red green blue alpha

rendTetramino :: Tetramino -> Picture -> Picture
rendTetramino I p = Color (makeColor 0.18 0.17 0.67 1) p
rendTetramino O p = Color (makeColor 0.68 0.17 0.67 1) p
rendTetramino T p = Color (makeColor 0.89 0.15 0.57 1) p
rendTetramino S p = Color (makeColor 0.97 0.08 0.40 1) p
rendTetramino Z p = Color (makeColor 0.97 0.86 0.08 1) p
rendTetramino J p = Color (makeColor 0    0.62 0.69 1) p
rendTetramino L p = Color (makeColor 0.67 1    0    1) p

arrayBasic :: [[Picture]] -> [Picture]
arrayBasic = foldr f [] where
  f :: [Picture] -> [Picture] -> [Picture]
  f xs ps = (fmap (Translate 0 blockSize) ps) ++ (foldr remBlankFold [] (zip [0..] xs))

remBlankFold :: (Float,Picture) -> [Picture] -> [Picture]
remBlankFold (_,Blank) ps = ps
remBlankFold (f,p)     ps = (Translate (f*blockSize) 0 p):(ps)
