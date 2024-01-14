module Main (main) where

import Control.Concurrent (forkIO)
import Data.List (genericLength)
import Graphics.UI.Fungen
import PlayFile
import Sound.ALUT

type OvenTimer = Int

data GameState =
    Menu [String]
  | Baking OvenTimer
  | Burning OvenTimer
  | Burned
  | Cooling
  | Ruined
  deriving (Show, Eq)

msPerTick :: Int
msPerTick = 40

msPerSecond :: Int
msPerSecond = 1000

textures :: FilePictureList
textures = [ ("assets/textures/tiled_wall_background.bmp", Nothing)
           -- texture has a problem and doesn't load
           --, ("assets/textures/meatloaf_baking.bmp", Nothing)
           , ("assets/textures/meatloaf_burned.bmp", Nothing)
           , ("assets/textures/meatloaf_perfect.bmp", Nothing)
           ]

maxTextureId :: Int
maxTextureId = genericLength textures

secondsToTicks :: Int -> Int
secondsToTicks seconds = seconds * msPerSecond `div` msPerTick

setTimer :: IOGame t s u v Int
setTimer = randomInt (minTicks, maxTicks)
           where minTicks = secondsToTicks 30
                 maxTicks = secondsToTicks 180 -- 3 minutes

initialGameState :: GameState
initialGameState = Menu
  [ "Press k to page."
  , "Funky Kong likes to bake meatloaf."
  , "Unfortunately..."
  , "his oven is defunkt."
  , "It cooks very inconsistently."
  , "Once you start the game..."
  , "you can press k at any time to take it out."
  , "But don't take it out before it's done."
  , "And at all costs..."
  , "don't let it burn either."
  , "Ok, you can start now. Press k to start."
  ]

windowConfiguration :: WindowConfig
windowConfiguration = ( (600, 200)
                      , (800, 500)
                      , "Funky Funky Meatloaf"
                      )

background :: GameMap Int
background = textureMap 0 50 50 250.0 250.0

inputs :: [InputBinding GameState s u v]
inputs = [(Char 'k', Press, takeMeatloafOutOfOven)]

update :: GameState -> GameState
update (Baking 0)     = Burning $ secondsToTicks 2
update (Baking time)  = Baking $ time - 1
update (Burning 0)    = Burned
update (Burning time) = Burning $ time - 1
update state          = state

draw :: IOGame GameState s u v ()
draw = getGameAttribute >>= (printString . showState)
       where showState :: GameState -> String
             showState (Menu (x:_)) = x
             showState (Baking _)   = "Baking..."
             showState (Burning _)  = "Meatloaf is burning!"
             showState Burned       = "Meatloaf burned. Game over."
             showState Ruined       = "You took the meat loaf out too early. Game over."
             showState Cooling      = "Fine, you won, but I'll get you next time!"
             showState _            = ""

printString :: String -> IOGame t s u v ()
printString s =
  printOnScreen s font position r g b
    where font     = TimesRoman24
          position = (5,400)
          (r,g,b)  = (0.5, 1.0, 1.0)

updateEffects :: GameState
              -> IOGame GameState () u v ()
updateEffects state = do
  let state' = update state
  setGameAttribute state'
  updateMonkey state'

updateMonkey :: GameState -> IOGame GameState () u v ()
updateMonkey state = do
  obj <- findObject "monkey" "monkey"
  replaceObject obj (updateObjectPicture textureId maxTextureId)
  where textureId = case state of
                      Burned  -> 1
                      Cooling -> 2
                      _       -> 0

takeMeatloafOutOfOven :: InputHandler GameState s u v
takeMeatloafOutOfOven _ _ = do
  state <- getGameAttribute
  case state of
    Menu [_]   -> Baking <$> setTimer >>= setGameAttribute
    Menu xs    -> setGameAttribute $ Menu $ tail xs
    Burning _  -> setGameAttribute Cooling
    Cooling    -> return ()
    Ruined     -> return ()
    Burned     -> return ()
    _          -> setGameAttribute Ruined

gameCycle :: IOGame GameState () u v ()
gameCycle = do
  state <- getGameAttribute
  if shouldTimerDing state
    then liftIOtoIOGame playTimerJingle
    else return ()
  updateEffects state
  draw

monkey :: ObjectManager ()
monkey = objectGroup "monkey" [createMonkey]

createMonkey :: GameObject ()
createMonkey = do
  let monkeyPic = Tex (300.0,300.0) 1
    in object "monkey" monkeyPic False (125, 125) (0, 0) ()

shouldTimerDing :: GameState -> Bool
shouldTimerDing (Baking 0) = True
shouldTimerDing _          = False

playTimerJingle :: IO ()
playTimerJingle = do
  _ <- forkIO $ withProgNameAndArgs runALUT $ \_ _ -> playFile ("assets/media/song.wav")
  return ()

main :: IO ()
main = do
  -- Initialise ALUT and eat any ALUT-specific commandline flags.
  --_ <- forkIO $ withProgNameAndArgs runALUT $ \_ _ -> playFile ("assets/media/song.wav")
  funInit windowConfiguration
          background
          [monkey]
          ()
          initialGameState
          inputs
          gameCycle
          (Timer msPerTick)
          textures 
