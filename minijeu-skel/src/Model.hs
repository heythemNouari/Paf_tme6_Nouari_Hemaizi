
module Model where

import SDL

import Keyboard (Keyboard)
import qualified Keyboard as K

data GameState = GameState { persoX :: Int
                           , persoY :: Int
                           , speed :: Int
                           , virusX :: Int 
                           , virusY :: Int }
  deriving (Show)


initGameState :: GameState
initGameState = GameState 200 300 4 500 400

moveLeft :: GameState -> GameState
moveLeft gs@(GameState px _ sp _ _) | px > 0 = gs { persoX = px - sp }
                                | otherwise = gs

moveRight :: GameState -> GameState
moveRight gs@(GameState px _ sp _ _) | px < 540 = gs { persoX = px + sp }
                                 | otherwise = gs

                              
moveUp :: GameState -> GameState
moveUp gs@(GameState _ py sp _ _) | py > 0 = gs { persoY = py - sp }
                              | otherwise = gs

moveDown :: GameState -> GameState
moveDown gs@(GameState _ py sp _ _) | py < 380 = gs { persoY = py + sp }
                                | otherwise = gs


gameStep :: RealFrac a => GameState -> Keyboard -> a -> GameState
gameStep gstate kbd deltaTime =
  -- A MODIFIFIER
  gstate
gameWin :: GameState -> Bool
gameWin gs@(GameState px py _ vx vy) = if px == vx && py == vy then True else False  