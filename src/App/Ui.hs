{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
module App.Ui (runUi) where

import           Lib.Minesweeper
import           App.Params

import           Control.Concurrent
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Array.IArray
import           Data.Default
import           Data.List
import           System.Random
import           Text.Printf

import           Brick.AttrMap
import qualified Brick.Main                 as M
import           Brick.Types
import qualified Brick.Types                as T
import           Brick.Util
import qualified Brick.Widgets.Border       as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center       as C
import           Brick.Widgets.Core
import qualified Graphics.Vty               as V

data St = St { _params    :: Params
             , _board     :: Board
             , _position  :: (Int, Int)
             , _time      :: Int
             , _gameState :: GameState
             , _timer     :: Timer
             }

type Timer = (Chan MyEvent, ThreadId)

data MyEvent = TimerEvent | VtyEvent V.Event

makeLenses ''St

vpTitle :: T.Name
vpTitle = "Minesweeper"

selectedAttr :: AttrName
selectedAttr = "selected"

drawUi :: St -> [Widget]
drawUi st = [ui]
    where ui = C.center $ withBorderStyle borderStyle $ B.border $
               vBox [ hLimit width $
                      hBox [str $ stateStr (st^.gameState)
                           , padLeft Max $ str $ printf "%04d" (st^.time)
                           ]
                    , padLeftRight (max 0 ((width - boardWidth - 2)) `div` 2) $
                      B.border boardVp
                    , str $ intercalate "\n" (instructions (st^.gameState))
                    ]
          stateStr :: GameState -> String
          stateStr Won = "You won!"
          stateStr Lost = "You lost!"
          stateStr Active = ""
          boardVp = hLimit boardWidth $
                    vLimit boardHeight $
                    viewport vpTitle Both $
                    hBox $ do
                        i <- [st^.board.minX .. st^.board.maxX]
                        let row = do
                              j <- [st^.board.minY .. st^.board.maxY]
                              let mkItem = if (i,j) == st^.position
                                           then withAttr selectedAttr . visible
                                           else id
                              return $ mkItem $ str $ printField $
                                  st^?!board.(ix (i,j).status)
                        return $ vBox row
          printField = if st^.params.(to useAscii) then printStatusA else printStatus
          instructions :: GameState -> [String]
          instructions Active = [ "- Arrows navigate the board"
                                , "- Space checks current field"
                                , "- 'm' marks current field"
                                , "- Esc exits the game"
                                ]
          instructions _      = [ "- 'r' restarts the game"
                                , "- "
                                , "- "
                                , "- Esc exits the game"
                                ]
          width = maximum [ (maximum . map length) (instructions Active)
                          , (maximum . map length) (instructions Won)
                          , length (stateStr Won) + 5
                          , length (stateStr Lost) + 5
                          , boardWidth + 2
                          ]
          boardWidth = st^.board.maxX - st^.board.minX + 1
          boardHeight = st^.board.maxY - st^.board.minY + 1
          borderStyle = if useAscii (st^.params) then BS.ascii else BS.unicode

appEvent :: St -> MyEvent -> T.EventM (T.Next St)
appEvent st (VtyEvent ev) = if st^.gameState == Active then
                                keyEvent st ev
                            else
                                endKeyEvent st ev
appEvent st TimerEvent = if st^.gameState == Active then
                             M.continue $ st & time %~ (+ 1)
                         else
                             M.continue st

keyEvent :: St -> V.Event -> T.EventM (T.Next St)
keyEvent st (V.EvKey V.KDown [])       = M.continue $ st & position._2 %~ min (st^.board.maxY) . (+ 1)
keyEvent st (V.EvKey V.KUp [])         = M.continue $ st & position._2 %~ max (st^.board.minY) . subtract 1
keyEvent st (V.EvKey V.KRight [])      = M.continue $ st & position._1 %~ min (st^.board.maxX) . (+ 1)
keyEvent st (V.EvKey V.KLeft [])       = M.continue $ st & position._1 %~ max (st^.board.minX) . subtract 1
keyEvent st (V.EvKey (V.KChar 'm') []) = M.continue $ st & board %~ snd.(runState $ mark (st^.position))
keyEvent st (V.EvKey (V.KChar ' ') []) = M.continue $ st & gameState .~ fst runCheck
                                                         & board     .~ snd runCheck
                                         where runCheck = runState (check (st^.position)) (st^.board)
keyEvent st (V.EvKey V.KEsc [])        = M.halt st
keyEvent st _                          = M.continue st

endKeyEvent :: St -> V.Event -> T.EventM (T.Next St)
endKeyEvent st (V.EvKey (V.KChar 'r') []) = do
    newTimer <- liftIO $ restartTimer (st^.timer)
    newState <- liftIO $ initState (st^.params) newTimer
    M.continue newState
endKeyEvent st (V.EvKey V.KEsc [])        = M.halt st
endKeyEvent st _                          = M.continue st

minX :: Getter Board Int
minX = to bounds._1._1

minY :: Getter Board Int
minY = to bounds._1._2

maxX :: Getter Board Int
maxX = to bounds._2._1

maxY :: Getter Board Int
maxY = to bounds._2._2

theMap :: AttrMap
theMap = attrMap V.defAttr
    [ (selectedAttr, V.black `on` V.yellow)
    ]

app :: M.App St MyEvent
app = M.App { M.appDraw = drawUi
            , M.appStartEvent = return
            , M.appHandleEvent = appEvent
            , M.appAttrMap = const theMap
            , M.appLiftVtyEvent = VtyEvent
            , M.appChooseCursor = M.neverShowCursor
            }

initState :: Params -> Timer -> IO St
initState params timer = do
    gen <- newStdGen
    let initialBoard = case boardType params of
                            Beginner       -> generateBeginner gen
                            Intermediate   -> generateIntermediate gen
                            Expert         -> generateExpert gen
                            (Custom w h m) -> generateBoard w h m gen
    return St { _params = params
              , _board = initialBoard
              , _position = (initialBoard^.minX, initialBoard^.minY)
              , _time = 0
              , _gameState = Active
              , _timer = timer
              }

initTimer :: Chan MyEvent -> IO Timer
initTimer chan = do
    threadId <- forkIO $ forever $ do
        threadDelay 1000000
        writeChan chan TimerEvent
    return (chan, threadId)

restartTimer :: Timer -> IO Timer
restartTimer (chan, threadId) = killThread threadId >> initTimer chan

runUi :: Params -> IO ()
runUi params = do
  chan <- newChan
  newTimer <- initTimer chan
  initialState <- initState params newTimer
  void $ M.customMain (V.mkVty def) chan app initialState
