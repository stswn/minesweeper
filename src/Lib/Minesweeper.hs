{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}
module Lib.Minesweeper ( Field
                       , FieldStatus(..)
                       , GameState(..)
                       , Board
                       , BoardStatus
                       , Game
                       , boardStatus
                       , mark
                       , check
                       , printStatus
                       , printStatusA
                       , generateBoard
                       , generateBeginner
                       , generateIntermediate
                       , generateExpert
                       , runState
                       ) where

import           Control.Lens
import           Control.Monad
import           Control.Monad.Trans.State.Lazy
import           Data.Array.IArray
import           Data.List
import           System.Random

data Field = Field { _isMine :: Bool
                   , _status :: FieldStatus
                   }

data FieldStatus = Virgin | Checked Int | Marked | Exploded

data GameState = Active | Won | Lost deriving Eq

type Board = Array (Int, Int) Field

type BoardStatus = Array (Int, Int) FieldStatus

type Game m = StateT Board m

makeLenses ''Field

boardStatus :: Board -> BoardStatus
boardStatus = fmap _status

mark :: Monad m => (Int, Int) -> Game m ()
mark i = do
    board <- get
    case board^?!(field i) of
        Virgin    -> (field i) .= Marked
        Marked    -> (field i) .= Virgin
        otherwise -> return ()

check :: Monad m => (Int, Int) -> Game m GameState
check i = do
    board <- get
    case board^?!(field i) of
        Checked _ -> return Active
        Exploded  -> return Lost
        otherwise -> do
            if board^?!(isFieldMine i) then do
                mines .= Exploded
                return Lost
            else do
                m <- countMines i
                (field i) .= Checked m
                when (m == 0) $ do
                    neighbours <- neighbourhood i
                    mapM_ check neighbours
                end <- boardFinished
                if end then return Won else return Active

field :: (Int, Int) -> Traversal' Board FieldStatus
field i = (ix i).status

isFieldMine :: (Int, Int) -> Traversal' Board Bool
isFieldMine i = (ix i).isMine

countMines :: Monad m => (Int, Int) -> Game m Int
countMines i = do
    board <- get
    neighbours <- neighbourhood i
    return $ length $ filter (\n -> board^?!(isFieldMine n)) neighbours

mines :: Traversal' Board FieldStatus
mines = traverse.(filtered _isMine).status

boardFinished :: Monad m => Game m Bool
boardFinished = do
    board <- get
    return $ all (\f -> case f^.status of (Checked _) -> True
                                          otherwise   -> f^.isMine) board

neighbourhood :: Monad m => (Int, Int) -> Game m [(Int, Int)]
neighbourhood (x, y) = do
    ((minx, miny), (maxx, maxy)) <- gets bounds
    return [(xp, yp) | xp <- [x - 1, x, x + 1]
                     , yp <- [y - 1, y, y + 1]
                     , xp >= minx, xp <= maxx
                     , yp >= miny, yp <= maxy
                     , xp /= x || yp /= y
                     ]

instance (Random x, Random y) => Random (x, y) where
    randomR ((x1, y1), (x2, y2)) gen1 =
        let (x, gen2) = randomR (x1, x2) gen1
            (y, gen3) = randomR (y1, y2) gen2
        in ((x, y), gen3)
    random gen1 =
        let (x, gen2) = random gen1
            (y, gen3) = random gen2
        in ((x, y), gen3)

generateBoard :: RandomGen g => Int -> Int -> Int -> g -> Board
generateBoard x y numOfMines g =
    let mines = take numOfMines $ nub $ randomRs ((0,0), (x-1,y-1)) g
        genField = \i ->
            if i `elem` mines then Field True Virgin else Field False Virgin
    in array ((0,0), (x-1,y-1)) [((i,j), genField (i,j)) | i <- [0..x-1], j <- [0..y-1]]

generateBeginner :: RandomGen g => g -> Board
generateBeginner = generateBoard 9 9 10

generateIntermediate :: RandomGen g => g -> Board
generateIntermediate = generateBoard 16 16 40

generateExpert :: RandomGen g => g -> Board
generateExpert = generateBoard 30 16 99

printStatus :: FieldStatus -> String
printStatus Virgin = "░"
printStatus (Checked 0) = " "
printStatus (Checked i) = show i
printStatus Marked = "⚑"
printStatus Exploded = "⚙"

printStatusA :: FieldStatus -> String
printStatusA Virgin = "░"
printStatusA (Checked 0) = " "
printStatusA (Checked i) = show i
printStatusA Marked = "~"
printStatusA Exploded = "o"
