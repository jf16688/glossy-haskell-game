module Renderer (game2Pic) where

import LibGame
import qualified Data.Map.Lazy as M

blockSize :: Float
blockSize = 40

basicBlock :: Float -> Float -> Picture
basicBlock x y = (Polygon [(0,0),(0,y),(x,y),(x,0)])

game2Pic :: Game -> Picture
game2Pic (Menu x _) = rendMenu x
game2Pic (Play {for = f, opacity = b, fall = p, word = w, foundChars = cs}) = Translate (-xLength/2) (-yLength/2) (Pictures (bg ++ rendBack b yLength ++ rendFor f yLength ++ rendFall p yLength ++ (fmap (Translate (xLength) (10)) (rendWord w)) ++ (fmap (Translate (xLength) (yLength/2)) (rendWord cs)))) where
  --bArr    = unOpacity b
  yLength = (fromIntegral worldHeight) * blockSize
  xLength = (fromIntegral worldWidth) * blockSize
  bg :: [Picture]
  bg = [Color (makeColor 0.95 0.95 0.95 1) (basicBlock xLength yLength)]

rendMenu :: Menu -> Picture
rendMenu i = Pictures [Color (red) (Polygon [(-220,-50),(220,-50),(220,50),(-220,50)]),
                              Translate (-205) (-25) (Scale 0.2 0.2  (Text s)),
                              Color (blue) (Line [(-220,-50),(220,-50),(220,50),(-220,50),(-220,-50)])
                             ]
  where
    s = case i of
      M 1 -> "Press Space to Play Game"
      GameOver s wtf g -> "Final Guess: " ++ s -- ++ "\n" ++ g
      Lose s -> "You Lose, word was: " ++ s
      M 3 -> "YOU WIN!!!!"
      _ -> "Something went wrong"

rendBack :: Opacity -> Float -> [Picture]
rendBack (Opacity ps) s = (fmap (f s) (M.toList ps)) where
  f :: Float -> ((Int,Int),Int) -> Picture
  f s ((y,x),o) = Color (makeColor 0 0 0 (1 - g o)) (Translate ((fromIntegral x) * blockSize) (s - (fromIntegral y) * blockSize) (basicBlock blockSize blockSize))
  g :: Int -> Float
  g x = exp (-(((fromIntegral x))/ 120))

rendFor :: Fg -> Float -> [Picture]
rendFor (Fg ps) s = (fmap (f s)) ps where
  f :: Float -> ((Int,Int),Tetramino) -> Picture
  f s ((y,x) ,t) = rendTetramino t (Translate ((fromIntegral x) * blockSize) (s - (fromIntegral y) * blockSize) (basicBlock blockSize blockSize))

rendFall :: FallingBlock -> Float -> [Picture]
rendFall b@(FallingBlock tet _ _) t = ((fmap ((rendTetramino tet) . (f t))) . blockPoints) b where
  f :: Float -> (Int,Int) -> Picture
  f top (y,x) = Translate ((fromIntegral x) * blockSize) (top - ((fromIntegral y) * blockSize)) (basicBlock blockSize blockSize)

rendWord :: String -> [Picture]
rendWord s = [Scale 0.4 0.4 (Text s)]

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
