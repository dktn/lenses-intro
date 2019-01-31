{-# LANGUAGE TemplateHaskell #-}

module Example2 where

import Control.Lens

data Point a =
  Point
    { _x :: a
    , _y :: a
    } deriving (Show)

makeLenses ''Point

data Line a =
  Line
    { _lineStart :: Point a
    , _lineEnd   :: Point a
    } deriving (Show)

makeLenses ''Line

makePoint :: (a, a) -> Point a
makePoint (x, y) = Point x y

makeLine :: (a, a) -> (a, a) -> Line a
makeLine start end = Line (makePoint start) (makePoint end)

run :: IO ()
run = do
  putStrLn "Example 2 - start"

  -- access nested property
  let line1 = makeLine (1, 2) (3, 4)
  putStrLn $ "line1: " <> show line1
  putStrLn $ "line1.lineEnd.x: " <> show (line1 ^. lineEnd . x)

  -- modify top level property
  let line2 = line1 & lineStart .~ makePoint (0, 1)
  putStrLn $ "line2: " <> show line2
  -- create without using accessors
  let line3 = Line (Point 1 3) (Point 3 4)
  putStrLn $ "line3: " <> show line3
  -- create using accessors (same thing)
  let line4 = Line { _lineStart = Point { _x = 1, _y = 2 }, _lineEnd = Point { _x = 3, _y = 4 } }
  putStrLn $ "line4: " <> show line4
  let line5 =
        Line
          { _lineStart = Point { _x = 1, _y = 2 }
          , _lineEnd   = Point { _x = 3, _y = 4 }
          }
  putStrLn $ "line5: " <> show line5

  -- multiply endline.x by 2, no helper variable needed
  let
      line6 = line1 & lineEnd . y %~ (*2)
  putStrLn $ "line6: " <> show line6

  -- multiply endline.x by 2, no helper variable, same code
  let line7 = line1 & lineEnd . y %~ (*2)
  putStrLn $ "line7: " <> show line7

  putStrLn "Example 2 - end"
