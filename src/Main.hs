module Main (main) where

import Data.List (genericLength)
import Graphics.UI.Fungen
import Media
import Sound.ALUT

type OvenTimer = Int

data GameStage =
    Menu [String]
  | Baking OvenTimer
  | Burning OvenTimer
  | Burned
  | Cooling
  | Ruined
  deriving (Show, Eq)

data GameState = GameState
  { getStage     :: GameStage
  , getMainTheme :: Source
  , getTimerDing :: Source
  } deriving Show

setStage :: GameStage -> GameState -> GameState
setStage stage' (GameState _ mainTheme timerDing) = GameState stage' mainTheme timerDing

msPerTick :: Int
msPerTick = 100

msPerSecond :: Int
msPerSecond = 1000

textures :: FilePictureList
textures = [ ("assets/textures/tiled_wall_background.bmp", Nothing)
           , ("assets/textures/meatloaf_baking.bmp", Nothing)
           , ("assets/textures/meatloaf_burned.bmp", Nothing)
           , ("assets/textures/meatloaf_perfect.bmp", Nothing)
           , ("assets/textures/meatloaf_undercooked.bmp", Nothing)
           , ("assets/textures/meatloaf_instructions.bmp", Nothing)
           ]

maxTextureId :: Int
maxTextureId = genericLength textures

secondsToTicks :: Int -> Int
secondsToTicks seconds = seconds * msPerSecond `div` msPerTick

setTimer :: IOGame t s u v Int
setTimer = randomInt (minTicks, maxTicks)
           where minTicks = secondsToTicks 30
                 maxTicks = secondsToTicks 180 -- 3 minutes

menu :: GameStage
menu = Menu
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

setupGameState :: IO GameState
setupGameState = do
  mainTheme <- createMainThemeSource "assets/media/main_theme.wav"
  timerDing <- createTimerDingSource "assets/media/bell_ding.wav"
  return $ GameState menu mainTheme timerDing

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
update state = let stage' = updateStage (getStage state)
               in setStage stage' state

updateStage :: GameStage -> GameStage
updateStage (Baking 0)     = Burning $ secondsToTicks 2
updateStage (Baking time)  = Baking $ time - 1
updateStage (Burning 0)    = Burned
updateStage (Burning time) = Burning $ time - 1
updateStage state          = state

draw :: IOGame GameState s u v ()
draw = getGameAttribute >>= (printString . showState . getStage)
       where showState :: GameStage -> String
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
  return ()

playMainTheme :: GameState -> IO ()
playMainTheme state = do
  let mainTheme = getMainTheme state
  isPlaying <- (==Playing) <$> sourceState mainTheme
  if (not isPlaying) then
    play [mainTheme]
  else
    return ()

stopMainTheme :: GameState -> IO ()
stopMainTheme state = do
  let mainTheme = getMainTheme state
  stop [mainTheme]


updateMonkey :: GameState -> IOGame GameState () u v ()
updateMonkey state = do
  obj <- findObject "monkey" "monkey"
  replaceObject obj (updateObjectPicture textureId maxTextureId)
  where textureId = case (getStage state) of
                      Menu _  -> 5
                      Burned  -> 2
                      Cooling -> 3
                      Ruined  -> 4
                      _       -> 1

takeMeatloafOutOfOven :: InputHandler GameState s u v
takeMeatloafOutOfOven _ _ = do
  state <- getGameAttribute
  case getStage state of
    Menu [_]   -> (\time -> setStage (Baking time) state) <$> setTimer >>= setGameAttribute
    Menu xs    -> setGameAttribute $ setStage (Menu $ tail xs) state
    Burning _  -> setGameAttribute $ setStage Cooling state
    Cooling    -> return ()
    Ruined     -> return ()
    Burned     -> return ()
    _          -> setGameAttribute $ setStage Ruined state

gameCycle :: IOGame GameState () u v ()
gameCycle = do
  state <- getGameAttribute
  if shouldTimerDing $ getStage state
    then liftIOtoIOGame $ playTimerDing state
    else return ()
  if shouldMainThemePlay $ getStage state
    then liftIOtoIOGame $ playMainTheme state
    else liftIOtoIOGame $ stopMainTheme state
  updateEffects state
  draw

shouldMainThemePlay :: GameStage -> Bool
shouldMainThemePlay (Baking _)  = True
shouldMainThemePlay (Burning _) = True
shouldMainThemePlay Burned      = True
shouldMainThemePlay Ruined      = True
shouldMainThemePlay _           = False

monkey :: ObjectManager ()
monkey = objectGroup "monkey" [createMonkey]

createMonkey :: GameObject ()
createMonkey = do
  let monkeyPic = Tex (300.0,300.0) 1
    in object "monkey" monkeyPic False (400, 200) (0, 0) ()

shouldTimerDing :: GameStage -> Bool
shouldTimerDing (Baking 0) = True
shouldTimerDing _          = False

playTimerDing :: GameState -> IO ()
playTimerDing state = do
  let timerDing = getTimerDing state
  play [timerDing]

main :: IO ()
main = do
  withProgNameAndArgs runALUT $ \_ _ -> runGame

runGame :: IO ()
runGame = do
  initialGameState <- setupGameState
  funInit windowConfiguration
          background
          [monkey]
          ()
          initialGameState
          inputs
          gameCycle
          (Timer msPerTick)
          textures 
