{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}

module Rythm where

import Control.Monad (guard)
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq, ViewL(..), (><))
import System.Random (randomRIO)

import qualified Data.Sequence as S
import Lens.Micro.TH (makeLenses)
import Lens.Micro ((&), (.~), (%~), (^.))
import Linear.V2 (V2(..), _x, _y)

import Numeric (showFFloat)
-- Types

data Game = Game
  { _keys   :: [Coord]     -- ^coordinates where you hit the notes 
  , _notes  :: Seq Coord   -- ^Position of current notes to hit
  , _stats  :: Stats       -- ^Player stats (score, hit%, etc)
  , _nexts  :: Seq Note    -- ^Stream of random next notes
  , _paused :: Bool        -- ^Paused flag
  , _frozen :: Bool        -- ^Freeze to disallow duplicate turns between time steps
  , _dead   :: Bool        -- ^Game over flag
  }
  deriving (Show)

data Stats = Stats 
  { _score    :: Int -- ^ Game Score
  , _hits     :: Int -- ^ Amount of correct key triggers
  , _misses   :: Int -- ^ Amount of failed key triggers
  , _streak   :: Int -- ^ Player note streak
  , _duration :: Int -- ^ Length of game
  , _total    :: Int -- ^ Amount of notes generated
  } deriving Show

printStats :: Stats -> String
printStats s = 
  let 
    hitsF = fromIntegral $ _hits s
    missesF = fromIntegral $ _misses s
    totalF = fromIntegral $ _total s
    accuracy = hitsF / (hitsF + missesF)
    hitRate = hitsF / totalF
  in
    "Score: " ++ show (_score s)
     ++ "\nStreak: " ++ show (_streak s) 
     ++ "\nTotal: " ++ show (_total s)
     ++ "\nHits: " ++ show (_hits s)
     ++ "\nMisses: " ++ show (_misses s)
     ++ "\nAccuracy: " ++ showFFloat (Just 2) (accuracy * 100) "%"
     ++ "\nHitRate: " ++ showFFloat (Just 2) (hitRate * 100) "%"
  

data Position -- Left, Center Left, Center, Center Right, Right
  = L 
  | CL
  | C
  | CR
  | R
  deriving (Eq, Show, Enum, Bounded)
  
data Note
  = Silence
  | Single Position
  | Interval (Position, Position)
  | Chord (Position, Position, Position)
  deriving Show

data Stream a = a :| Stream a
  deriving (Show)

type Coord = V2 Int


makeLenses ''Game
makeLenses ''Stats
-- Constants

height, width, keyHeight:: Int
height = 30
width  = 5
keyHeight = 3

dec = \x -> x - 1

-- Decides coords of new note given its position
pos2note :: Position -> Coord
pos2note p = V2 (fromEnum p) (height - 1)

-- Translates key position to coord
pos2key :: Position -> Coord
pos2key p = V2 (fromEnum p) keyHeight

-- Recovers position from column
note2pos :: Coord -> Position
note2pos (V2 0 _) = L
note2pos (V2 1 _) = CL
note2pos (V2 2 _) = C
note2pos (V2 3 _) = CR
note2pos (V2 4 _) = R

-- | Step forward in time
step :: Game -> Game
step g = fromMaybe g $ do
  guard (not $ g ^. paused || g ^. dead)
  let g' = g & frozen .~ False
  return . fromMaybe (move g') $ die g'

-- | TODO: decide on losing condition, could be a hitrate threshold
die :: Game -> Maybe Game
die g = do
  guard (g ^. (stats.total) == g ^. (stats.duration))
  return $ g & dead .~ True

move :: Game -> Game
move g = advanceAll $ addNote g 

-- | Advance all notes 
advanceAll :: Game -> Game
advanceAll g =
    case missedNote g of
        True ->
            g & (stats . streak) .~ 0
              & (stats . misses) %~ (+(amtMissed $ g ^. notes))
              & (stats . total) %~ (+(amtMissed $ g ^. notes))
              & notes %~ (\s -> S.filter reachable $ fmap (\p -> p &_y %~ dec) s)
        False ->
            g & notes %~ \s -> fmap (\p -> p &_y %~ dec) s
    where
        reachable (V2 x y) = y >= keyHeight
        nonReachable (V2 x y) = y < keyHeight
        amtMissed s = length $ (S.filter nonReachable $ fmap (\p -> p &_y %~ dec) s)

missedNote :: Game -> Bool
missedNote g = 
    any nonReachable (g ^. notes)
    where
        nonReachable (V2 x y) = y < keyHeight

-- | Adds new random note to the game
addNote :: Game -> Game
addNote g = 
  case null $ g ^. nexts of
    True -> 
      g
    False ->
      g & notes %~ (flip (><) $ newCoords note) 
        & nexts .~ s
      where 
        (note :< s) = S.viewl (g ^. nexts)
        newCoords :: Note -> Seq Coord
        newCoords n =
          case n of
            Silence -> S.empty
            Single p -> S.singleton $ pos2note p
            Interval (p1, p2)  -> S.fromList $ fmap (pos2note) [p1, p2]
            Chord (p1, p2, p3) -> S.fromList $ fmap (pos2note) [p1, p2, p3]

-- Trigger a key and checks if it hit a note
trigger :: Game -> Position -> Game
trigger g p = 
  case (pos2key p) `elem` (g ^. notes) of
    True ->
      g & stats . score  %~ (+100)
        & stats . streak %~ (+1)
        & stats . total  %~ (+1)
        & stats . hits %~ (+1)
        & notes  %~ (S.filter (/= pos2key p))
    False ->
      g & stats . streak .~ 0
        & stats . misses %~ (+1)

----------------------------------------------------------------
initGame :: Int -> IO Game
initGame n = do
  inf <- sequence $ take n $ repeat nextNote
  let s = Stats
          { _score  = 0
          , _total  = 0
          , _streak = 0
          , _duration = sum $ countNotes <$> inf
          , _hits = 0
          , _misses = 0
          }
  let g = Game 
            { _keys   = pos2key <$> positions
            , _notes  = S.empty
            , _stats  = s
            , _nexts  = S.fromList inf
            , _dead = False, _paused = False , _frozen = False 
            }
  return g
  where
    countNotes :: Note -> Int
    countNotes Silence = 0
    countNotes (Single _) = 1
    countNotes (Interval _) = 2
    countNotes (Chord _) = 3
  
--Generating random notes

positions :: [Position]
positions = [L, CL, C, CR, R]

oneNote :: [Note]
oneNote = fmap Single $ positions

twoNote :: [Note]
twoNote = fmap Interval $ 
         [(positions !! x, positions !! y) 
          | x <- [0..length positions - 2]
          , y <- [x+1..length positions - 1]
         ]

threeNote :: [Note]
threeNote = fmap Chord $
           [(positions !! x, positions !! y, positions !! z)
            | x <- [0..length positions - 3]
            , y <- [x+1..length positions - 2]
            , z <- [y+1..length positions - 1]
           ]

noteClass :: [[Note]]
noteClass = [[Silence], oneNote, twoNote, threeNote]


nextNote :: IO Note
nextNote = do
    idx <- randomRIO(0, (length noteClass) - 1)
    jdx <- randomRIO(0, length (noteClass !! idx) - 1)
    return $ noteClass !! idx !! jdx 