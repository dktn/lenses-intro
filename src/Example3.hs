{-# LANGUAGE TemplateHaskell #-}

module Example3 where

import Control.Lens
import Data.Tuple (swap)

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

swapPoint :: Point a -> Point a
swapPoint (Point x y) = Point y x

swapPoint' :: Point a -> Point a
swapPoint' p = Point (p ^. y) (p ^. x)

test1 :: IO ()
test1 = do
  let line1 = makeLine (1, 2) (3, 4)
  putStrLn $ "line1: " <> show line1
  putStrLn "simple view"
  print $ view lineEnd line1
  print $ line1 ^. lineEnd
  putStrLn "simple set"
  print $ set lineEnd (makePoint (0, 1)) line1
  print $ lineEnd .~ makePoint (0, 1) $ line1
  print $ line1 & lineEnd .~ makePoint (0, 1)
  putStrLn "lenses composition"
  print $ view (lineEnd . y) line1
  print $ line1 ^. lineEnd . y
  putStrLn "value modification"
  print $ over (lineEnd . y) (*2) line1
  print $ lineEnd . y %~ (*2) $ line1
  print $ line1 & lineEnd . y %~ (*2)

test2 :: IO ()
test2 = do
  let tuple1 = (3, (5, 7))
  putStrLn $ "tuple1: " <> show tuple1
  putStrLn "simple view"
  print $ view _2 tuple1
  print $ tuple1 ^. _2
  putStrLn "simple set"
  print $ set _2 "new-value" tuple1
  print $ _2 .~ "new-value" $ tuple1
  print $ tuple1 & _2 .~ "new-value"
  putStrLn "lenses composition"
  print $ view (_2 . _1) tuple1
  print $ tuple1 ^. _2 . _1
  putStrLn "value modification"
  print $ over (_2 . _1) (*2) tuple1
  print $ _2 . _1 %~ (*2) $ tuple1
  print $ tuple1 & _2 . _1 %~ (*2)

test3 :: IO ()
test3 = do
  let tuple1 = (3, 5)
  putStrLn $ "tuple1: " <> show tuple1
  putStrLn "simple set"
  print $ set both "new-value" tuple1
  print $ both .~ "new-value" $ tuple1
  print $ tuple1 & both .~ "new-value"
  putStrLn "value modification"
  print $ over both (*2) tuple1
  print $ both %~ (*2) $ tuple1
  print $ tuple1 & both %~ (*2)

test4 :: IO ()
test4 = do
  let tuple1 = ("Hello, ", "world!")
  putStrLn $ "tuple1: " <> show tuple1
  putStrLn "simple view"
  print $ view both tuple1
  print $ tuple1 ^. both

test5 :: IO ()
test5 = do
  let tuple1 = (3, (5, 7))
  putStrLn $ "tuple1: " <> show tuple1
  putStrLn "value modification"
  print $ tuple1 & _2 . _1 %~ (*2)
  print $ tuple1 & _2 . _1 *~ 2
  print $ tuple1 & _2 . _1 *~ 2
                 & _2 . _2 +~ 3
                 & _1      -~ 1
                 & swap

test6 :: IO ()
test6 = do
  let line1 = makeLine (1, 2) (3, 4)
  putStrLn $ "line1: " <> show line1
  putStrLn "value modification"
  print $ line1 & lineStart . x *~ 2
                & lineStart . y +~ 3
                & lineEnd       %~ (\(Point x y) -> Point y x)

run :: IO ()
run = sequence_ [ test1, test2, test3, test4, test5, test6 ]
