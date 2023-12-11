{-# LANGUAGE OverloadedStrings #-}

module Main where
import Snake
import Control.Monad
import Foreign.C.Types
import SDL.Vect
import Control.Concurrent (threadDelay)
import SDL (($=))
import qualified SDL
import Control.Lens

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (640, 480)


render :: SDL.Renderer -> Snake -> IO ()
render renderer snake = do
    SDL.rendererDrawColor renderer $= V4 maxBound maxBound maxBound maxBound
    SDL.clear renderer
    mapM_ (\part -> do
            SDL.rendererDrawColor renderer $= V4 255 0 0 255
            SDL.fillRect renderer $ Just part
        ) $ snake ^. body
    SDL.present renderer

loop :: SDL.Renderer -> Snake -> IO ()
loop renderer snake = do
    events <- map SDL.eventPayload <$> SDL.pollEvents
    let quit = SDL.QuitEvent `elem` events
    let snake' = changeSnakeState events snake
    render renderer snake' 
    threadDelay (16 * 1000) 
    unless quit $ loop renderer snake' 



sdlClear :: SDL.Renderer -> SDL.Window -> IO ()
sdlClear renderer window = do
    SDL.destroyRenderer renderer
    SDL.destroyWindow window
    SDL.quit

sdlInit :: IO (SDL.Renderer, SDL.Window)
sdlInit = do
    SDL.initialize [SDL.InitVideo]

    SDL.HintRenderScaleQuality $= SDL.ScaleLinear
    do
        renderQuality <- SDL.get SDL.HintRenderScaleQuality
        when (renderQuality /= SDL.ScaleLinear) $
            putStrLn "Warning: Linear texture filtering not enabled!"

    window <-
        SDL.createWindow
        "Snake LOL 🤡"
        SDL.defaultWindow {SDL.windowInitialSize = V2 screenWidth screenHeight}
    SDL.showWindow window

    renderer <-
        SDL.createRenderer
        window
        (-1)
        SDL.RendererConfig
            { SDL.rendererType = SDL.AcceleratedRenderer
            , SDL.rendererTargetTexture = False
            }

    SDL.rendererDrawColor renderer $= V4 maxBound maxBound maxBound maxBound

    return (renderer, window)

main :: IO ()
main = do
    (renderer, window) <- sdlInit

    loop renderer $ createSnake $ V2 100 100

    sdlClear renderer window