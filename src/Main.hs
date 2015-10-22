{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module Main where

import Minesweeper

import Control.Lens
import Control.Monad
import Data.Array.IArray
import Data.List
import System.Random

import qualified Graphics.Vty as V
import qualified Brick.Types as T
import qualified Brick.Main as M
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Border as B
import Brick.AttrMap
import Brick.Util
import Brick.Types
import Brick.Widgets.Core

data St = St { _board :: Board
             , _position :: (Int, Int)
             }

makeLenses ''St

vpTitle :: T.Name
vpTitle = "Minesweeper"

selectedAttr :: AttrName
selectedAttr = "selected"

drawUi :: St -> [Widget]
drawUi st = [ui]
    where ui = C.center $
               vBox [B.border $ boardVp
                    , str $ intercalate "\n" instructions
                    ]
          boardVp = hLimit (st^.board.maxX - st^.board.minX + 1) $
                    vLimit (st^.board.maxY - st^.board.minY + 1) $
                    viewport vpTitle Both $
                    hBox $ do
                        i <- [st^.board.minX .. st^.board.maxX]
                        let row = do
                              j <- [st^.board.minY .. st^.board.maxY]
                              let mkItem = if (i,j) == st^.position
                                           then withAttr selectedAttr . visible
                                           else id
                              return $ mkItem $ str $ printStatus $
                                  st^?!board.(to boardStatus).(ix (i,j))
                        return $ vBox row
          instructions = [ "- Arrow keys navigate the board"
                         , "- Space key checks current field"
                         , "- m key marks current field"
                         , "- Esc exits the game"
                         ]

appEvent :: St -> V.Event -> T.EventM (T.Next St)
appEvent st (V.EvKey V.KDown [])         = M.continue $ st & position._2 %~ min (st^.board.maxY) . (+ 1)
appEvent st (V.EvKey V.KUp [])           = M.continue $ st & position._2 %~ max (st^.board.minY) . subtract 1
appEvent st (V.EvKey V.KRight [])        = M.continue $ st & position._1 %~ min (st^.board.maxX) . (+ 1)
appEvent st (V.EvKey V.KLeft [])         = M.continue $ st & position._1 %~ max (st^.board.minX) . subtract 1
appEvent st (V.EvKey (V.KChar 'm') [])   = M.continue $ st & board %~ snd.(runState $ mark (st^.position))
appEvent st (V.EvKey (V.KChar ' ') [])   = M.continue $ st & board %~ snd.(runState $ check (st^.position))
appEvent st (V.EvKey V.KEsc []) = M.halt st
appEvent st _ = M.continue st

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

app :: M.App St V.Event
app = M.App { M.appDraw = drawUi
            , M.appStartEvent = return
            , M.appHandleEvent = appEvent
            , M.appAttrMap = const theMap
            , M.appLiftVtyEvent = id
            , M.appChooseCursor = M.neverShowCursor
            }

main :: IO ()
main = do
  gen <- newStdGen
  let initialBoard = generateExpert gen
  let initialState = St initialBoard (initialBoard^.minX, initialBoard^.minY)
  void $ M.defaultMain app initialState
