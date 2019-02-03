{-# LANGUAGE TemplateHaskell #-}

module Example4 where

import Data.Monoid (Sum (..))
import Control.Lens
import Control.Monad.State (execState, State)

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

posX :: (Num a, Functor f) => (a -> f a) -> (Point a) -> f (Point a)
posX f (Point x y) = fmap (\x' -> Point x' y) (f x)

posY :: (Num a, Functor f) => (a -> f a) -> (Point a) -> f (Point a)
posY f (Point x y) = fmap (\y' -> Point x y') (f y)

pointCoordinates :: (Num a, Applicative f) => (a -> f a) -> (Point a) -> f (Point a)
pointCoordinates f (Point x y) = Point <$> f x <*> f y

modLine1 :: Num a => State (Line a) ()
modLine1 = do
  lineStart .= makePoint (0, 0)
  zoom lineEnd $ do
    x *= 2
    y += 5
    pointCoordinates %= negate

modLine2 :: Num a => State (Line a) ()
modLine2 = do
  zoom lineEnd $ do
    newX <- x <*= 5
    y .= newX

test1 :: IO ()
test1 = do
  let line1 = makeLine (1, 2) (3, 4)
  putStrLn $ "line1: " <> show line1
  print $ execState modLine1 line1
  print $ execState modLine2 line1

test2 :: IO ()
test2 = do
  let point1 = makePoint (1, 2)
  putStrLn $ "point1: " <> show point1
  print $ point1 ^. x
  print $ point1 ^. y
  print $ point1 ^. posX
  print $ point1 ^. posY
  print $ point1 & x .~ 10
  print $ point1 & y .~ 10
  print $ point1 & posX .~ 10
  print $ point1 & posY .~ 10
  print $ point1 & x +~ 10
  print $ point1 & y +~ 10
  print $ point1 & posX +~ 10
  print $ point1 & posY +~ 10

test3 :: IO ()
test3 = do
  let point1 = makePoint (Sum 7, Sum 8)
  putStrLn $ "point1: " <> show point1
  print $ point1 ^. pointCoordinates

test4 :: IO ()
test4 = do
  let point1 = makePoint (7, 8)
  putStrLn $ "point1: " <> show point1
  print $ point1 ^. to swapPoint
  print $ point1 & swapPoint

run :: IO ()
run = sequence_ [ test1, test2, test3, test4 ]
