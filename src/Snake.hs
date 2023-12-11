{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}
module Snake where

import qualified SDL
import Foreign.C
import Control.Lens
import SDL (Point(..))
import Linear (V2(..))
import Data.Void (absurd)


type SnakeBodyPart = SDL.Rectangle CInt
type SnakeBody = [SnakeBodyPart]
data Direction = Left | Right | Up | Down deriving (Show)

data Snake = Snake {
    _body :: SnakeBody,
    _direction :: Direction
} deriving (Show)

makeLenses ''Snake

createSnake :: V2 CInt -> Snake
createSnake (V2 x y) = Snake {
        _body = zipWith 
            (\_ i -> SDL.Rectangle (P $ V2 (x + i * 10) y) (V2 10 10)) 
            (replicate 5 absurd) [0..],
        _direction = Snake.Right
    }

moveBodyPart :: Direction -> SnakeBodyPart -> SnakeBodyPart
moveBodyPart dir (SDL.Rectangle (P (V2 x y)) size) =
    SDL.Rectangle (P newPos) size
        where newPos = case dir of
                Snake.Left -> V2 (x - 10) y
                Snake.Right -> V2 (x + 10) y
                Snake.Up -> V2 x (y - 10)
                Snake.Down -> V2 x (y + 10)
                

changeSnakeState :: [SDL.EventPayload] -> Snake -> Snake
changeSnakeState events snake = 
    
    let oldBody = snake ^. body
        newHead = moveBodyPart (snake ^. direction) (head oldBody)
        newBody = newHead : if null oldBody then [] else init oldBody

    in foldr (\e snake' -> case e of
        SDL.KeyboardEvent e' | SDL.keyboardEventKeyMotion e' == SDL.Pressed
            -> case SDL.keysymKeycode (SDL.keyboardEventKeysym e') of
                SDL.KeycodeLeft -> snake' & direction .~ Snake.Left
                SDL.KeycodeRight -> snake' & direction .~ Snake.Right
                SDL.KeycodeUp -> snake' & direction .~ Snake.Up
                SDL.KeycodeDown -> snake' & direction .~ Snake.Down
                _ -> snake'
        _ -> snake') (snake & body .~ newBody) events