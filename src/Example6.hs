{-# LANGUAGE TemplateHaskell #-}

module Example6 where

import Control.Lens
import Data.Map.Strict as Map
import Data.Monoid (Sum (..))
import Data.Char (chr, toTitle)

test1 :: IO ()
test1 = do
  print $ traverse %~ (*2) $ [1, 2, 3]
  print $ traverse %~ even $ [1, 2, 3]
  print $ traverse . traverse %~ length $ [["hello", "world"],["!!!"]]

test2 :: IO ()
test2 = do
  let tuple1 = (3, 5, 7)
  putStrLn $ "tuple1: " <> show tuple1
  print $ tuple1 & both %~ succ
  print $ tuple1 & each %~ succ
  let tuple2 = ("Quick", "brown", "fox")
  print $ tuple2 & both %~ length
  print $ tuple2 & each %~ length

test3 :: IO ()
test3 = do
  -- simple list
  let list1 = [1, 2, 3]
  print $ succ <$> list1
  print $ list1 & mapped %~ succ
  print $ mapped %~ succ $ list1
  print $ over mapped succ list1
  -- list of tuples
  let list2 = [(1, 2), (3, 4)]
  print $ list2 & mapped . _1 %~ succ
  print $ mapped . _1 %~ succ $ list2
  print $ over (mapped . _1) succ list2
  -- others
  print $ _1 . mapped . _2 . mapped %~ succ $ ([(1, "Quick"), (2, "brown")], "fox")
  print $ both <>~ "!!!" $ ("hello", "world")

test4 :: IO ()
test4 = do
  -- maps
  let map1 = Map.fromList [("paprica", "red"), ("carrot", "orange"), ("spinach", "green")]
  putStrLn $ "map1: " <> show map1
  print $ map1 & at "paprica" ?~ "yellow"
  print $ map1 & at "tomato" ?~ "red"
  print $ Map.empty & at "mis" ?~ "koala"
  print $ Nothing & id ?~ 7
  print $ map1 & at "paprica" .~ Just "green"
  print $ map1 & at "banana" .~ Just "yellow"
  print $ map1 & at "paprica" .~ Nothing

test5 :: IO ()
test5 = do
  print $ [66, 97, 116, 109, 97, 110] & each %~ (+1)
  print $ [66, 97, 116, 109, 97, 110] & each %%~ \a -> (Sum 1, a + 1)
  print $ [66, 97, 116, 109, 97, 110] & each %%~ \a -> ("na", chr a)

test6 :: IO ()
test6 = do
  print $ toListOf _1 (1, 2)
  print $ toListOf both (1, 2)
  print $ (1, 2) ^.. both
  print $ (1, 2) ^.. each
  print $ (1, 2, 3) ^.. both
  print $ (1, 2, 3) ^.. each
  print $ [[1, 2], [3]] ^. traverse
  print $ [[1, 2], [3]] ^.. id
  print $ [[1, 2], [3]] ^.. traverse
  print $ [[1, 2], [3]] ^.. traverse . traverse

test7 :: IO ()
test7 = do
  print $ preview _Left $ Left 4
  print $ Left 4 ^? _Left
  print $ (Right 4 ^? _Left :: Maybe Int)
  print $ "world" ^? ix 3
  print $ "world" ^? ix 20
  print $ "world" & ix 0 .~ 'W'
  print $ "world" & ix 5 .~ '!'

test8 :: IO ()
test8 = do
  let maximum0 = maximum & ix [] .~ 0
  print $ maximum [2, 3, 1]
  -- print $ (maximum [] :: Int) -- Why is it commented out?
  print $ maximum0 [2, 3, 1]
  print $ maximum0 []
  let maximumFunny = maximum & ix [2] .~ -7
  print $ maximumFunny [2, 3, 1]
  print $ maximumFunny [2]

test9 :: IO ()
test9 = do
  print $ [1..5] ^? _head
  print $ "a small fox." & _head %~ toTitle
  print $ [1..5] ^? _tail
  print $ [1..5] ^? _init
  print $ [1..5] ^? _last

run :: IO ()
run = sequence_ [ test1, test2, test3, test4, test5, test6, test7, test8, test9 ]
