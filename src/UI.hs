{-# LANGUAGE OverloadedStrings #-}
module UI where

import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay, forkIO)
import Data.Maybe (fromMaybe)

import Rythm

import Brick
  ( App(..), AttrMap, BrickEvent(..), EventM, Next, Widget
  , customMain, neverShowCursor
  , continue, halt
  , hLimit, vLimit, vBox, hBox
  , padRight, padLeft, padTop, padAll, Padding(..)
  , withBorderStyle
  , str
  , attrMap, withAttr, emptyWidget, AttrName, on, fg, bg
  , (<+>)
  )
import Brick.BChan (newBChan, writeBChan)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Graphics.Vty as V
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Linear.V2 (V2(..))
import Lens.Micro ((^.))

-- Types

-- | Ticks mark passing of time
data Tick = Tick

-- | Named resources
type Name = ()

data Cell = Key | Nota Position | Empty

-- App definition

app :: App Game Tick Name
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const theMap
          }

main :: IO ()
main = do
  putStrLn "Enter length of game: "
  n <- readLn :: IO Int
  chan <- newBChan 10
  forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay 500000 -- decides how fast your game moves
  g <- initGame n
  void $ customMain (V.mkVty V.defaultConfig) (Just chan) app g

-- Handling events

handleEvent :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)
handleEvent g (AppEvent Tick)                       = continue $ step g

handleEvent g (VtyEvent (V.EvKey (V.KChar 'f') [])) = continue $ trigger g L
handleEvent g (VtyEvent (V.EvKey (V.KChar 'g') [])) = continue $ trigger g CL
handleEvent g (VtyEvent (V.EvKey (V.KChar 'h') [])) = continue $ trigger g C
handleEvent g (VtyEvent (V.EvKey (V.KChar 'j') [])) = continue $ trigger g CR
handleEvent g (VtyEvent (V.EvKey (V.KChar 'k') [])) = continue $ trigger g R

handleEvent g (VtyEvent (V.EvKey (V.KChar 'r') [])) = liftIO (initGame (g ^. (stats.duration))) 
                                                        >>= continue
handleEvent g (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt g
handleEvent g (VtyEvent (V.EvKey V.KEsc []))        = halt g
handleEvent g _                                     = continue g

-- Drawing

drawUI :: Game -> [Widget Name]
drawUI g =
  [ C.center $ padRight (Pad 2) (drawStats g) <+> drawGrid g <+> padLeft (Pad 2) drawKeys ]

drawKeys :: Widget Name
drawKeys = hLimit 48
  $ vBox [ withBorderStyle BS.unicodeBold
            $ B.borderWithLabel (str "Controls")
            $ C.hCenter
            $ padAll 1
            $ str $ controls
         ]
controls :: String 
controls = "trigger keys (left to right): f g h j k\nReset: r\nQuit: q"

drawStats :: Game -> Widget Name
drawStats g = hLimit 20
  $ vBox [ drawScore (g ^. stats)
         , padTop (Pad 2) $ drawGameOver (g ^. dead)
         ]

drawScore :: Stats -> Widget Name
drawScore s = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "Stats")
  $ C.hCenter
  $ padAll 1
  $ str $ printStats s

drawGameOver :: Bool -> Widget Name
drawGameOver dead =
  if dead
     then withAttr gameOverAttr $ C.hCenter $ str "GAME OVER"
     else emptyWidget

drawGrid :: Game -> Widget Name
drawGrid g = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str " f| g| h| j| k")
  $ vBox rows
  where
    rows         = [hBox $ cellsInRow r | r <- [height-1,height-2..0]]
    cellsInRow y = [drawCoord (V2 x y) | x <- [0..width-1]]
    drawCoord    = drawCell . cellAt
    cellAt c
      | elem c $ g ^. notes = Nota (note2pos c)
      | c `elem` g ^. keys = Key
      | otherwise  = Empty

drawCell :: Cell -> Widget Name
drawCell Key = withAttr keyAttr kw
drawCell (Nota p) = withAttr (getNotaAttr p) cw
drawCell Empty = withAttr emptyAttr chord

cw :: Widget Name
cw = str " ◯ "

chord :: Widget Name
chord = str " : "

kw :: Widget Name
kw = str "   "

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (keyAttr, bg $ V.white)
  , (lAttr, bg V.green)
  , (clAttr, bg V.yellow)
  , (cAttr, bg V.blue )
  , (crAttr, bg V.magenta)
  , (rAttr, bg V.cyan)
  , (gameOverAttr, fg V.red `V.withStyle` V.bold)
  ]

gameOverAttr :: AttrName
gameOverAttr = "gameOver"

getNotaAttr :: Position -> AttrName
getNotaAttr L = lAttr
getNotaAttr CL = clAttr
getNotaAttr C = cAttr
getNotaAttr CR = crAttr
getNotaAttr R = rAttr

lAttr, clAttr, cAttr, crAttr, rAttr :: AttrName
lAttr = "lAttr"
clAttr = "clAttr"
cAttr = "cAttr"
crAttr = "crAttr"
rAttr = "rAttr"

keyAttr, emptyAttr :: AttrName
keyAttr = "keyAttr"
emptyAttr = "emptyAttr"

