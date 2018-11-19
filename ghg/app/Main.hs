module Main where

import Lib
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

data Game = Play {for   :: Foreground,
                  back  :: Background,
                  wtf   :: Word2Find,
                  fall  :: FallingBlock}
          | Menu {menu :: Menu}

data Tetramino = L | I | T | S | Z | B

data Foreground = Foreground [[Maybe Tetramino]]

data Background = Background [[Maybe (Char, Int)]]

data Word2Find = Word2Find String

data FallingBlock = FallingBlock Tetramino Int [(Int,Int)]

data Menu = M Int

main :: IO ()
main = play FullScreen black 60 (Menu (M 0)) game2Pic eventHandler worldStepper

game2Pic :: Game -> Picture
game2Pic g = Blank

eventHandler :: Event -> Game -> Game
eventHandler e g = g

worldStepper :: Float -> Game -> Game
worldStepper f g = g
