{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module UserInput (eventHandler) where

import Prelude hiding (Right, Left)
import qualified Prelude as P
import LibGame hiding (Rotate)
import Debug.Trace
-- import System.Random

data Direction = Left | Right deriving Show

data Move = Rotate Direction
          | Move Direction
          | FastDrop Bool
          | WordInput Char
          | GuessWord
          | Start
          deriving Show

eventHandler :: Event -> Game -> Game
eventHandler e = update (getMove e)

getMove :: Event -> Maybe Move
getMove (EventKey (SpecialKey key) Down _ _) =
  case key of
    KeyUp    -> Just (Rotate Right)
    -- KeyDown  -> Just (Rotate Left)
    KeySpace -> Just Start
    KeyRight -> Just (Move Right)
    KeyLeft  -> Just (Move Left)
    KeyDown -> Just (FastDrop True)
    KeyEnter -> Just GuessWord
    _        -> Nothing
-- getMove (EventKey (Char '(' Down _ _) = Just Start
getMove (EventKey (SpecialKey KeyDown) Up _ _) = Just $ FastDrop False

getMove (EventKey (Char c) Down _ _) = Just (WordInput c)
getMove _ = Nothing

rotateCompass Right North = East
rotateCompass Right East = South
rotateCompass Right South = West
rotateCompass Right West = North
rotateCompass Left n = rotateCompass Right . rotateCompass Right . rotateCompass Right $ n

movePoint Right c = addPointPoint c (0, 1)
movePoint Left c = addPointPoint c (0, -1)


update :: Maybe Move -> Game -> Game
-- update x y = traceShow (x, y) y
update (Just Start) g@Menu{rands} = initial_game rands
update (Just x) g@Play{fall = FallingBlock t c r, acceleration, wtf = Word2Find wtf, word, rands} =
  case traceShowId x of
    Rotate d -> changeIfNoCollision (FallingBlock t c (rotateCompass d r)) g
    Move d -> changeIfNoCollision (FallingBlock t (movePoint d c) r) g
    FastDrop True -> g { acceleration = acceleration * 10 } -- Bump the tick rate by some scaler
    FastDrop False -> g { acceleration = acceleration / 10 }
    WordInput c -> g { word = word ++ [c] }
    GuessWord -> if wtf == word then Menu (M 3) rands else g {word = ""}
    _ -> g
update _ g = g

changeIfNoCollision :: FallingBlock -> Game -> Game
changeIfNoCollision b g | hasCollided (for g) b = g
                        | wallCollision b       = g
                        | otherwise             = g {fall = b}
