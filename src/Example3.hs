{-# LANGUAGE TemplateHaskell #-}

module Example3 where

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

test1 :: IO ()
test1 = do
  let line1 = makeLine (1, 2) (3, 4)
  putStrLn $ "line1: " <> show line1
  -- simple view
  putStrLn $ "view lineEnd line1: " <> show (view lineEnd line1)
  putStrLn $ "lineEnd ^. line1:   " <> show (line1 ^. lineEnd)
  -- simple set
  putStrLn $ "set lineEnd (makePoint (0, 1)) line1: "  <> show (set lineEnd (makePoint (0, 1)) line1)
  putStrLn $ "lineEnd .~ (makePoint (0, 1)) $ line1: " <> show (lineEnd .~ (makePoint (0, 1)) $ line1)
  putStrLn $ "line1 & lineEnd .~ (makePoint (0, 1)): " <> show (line1 & lineEnd .~ (makePoint (0, 1)))
  -- lenses composition
  putStrLn $ "view (lineEnd . y) line1: " <> show (view (lineEnd . y) line1)
  putStrLn $ "line1 ^. lineEnd . y: "     <> show (line1 ^. lineEnd . y)
  -- value modification
  putStrLn $ "over (lineEnd . y) (*2) line1: " <> show (over (lineEnd . y) (*2) line1)
  putStrLn $ "lineEnd . y %~ (*2) $ line1: "   <> show (lineEnd . y %~ (*2) $ line1)
  putStrLn $ "line1 & lineEnd . y %~ (*2): "   <> show (line1 & lineEnd . y %~ (*2))

run :: IO ()
run = test1
