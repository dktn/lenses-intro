{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Example8 where

import Control.Lens

data Department =
  Department
    { _number :: Int
    , _name   :: String
    } deriving (Show)

makeFieldsNoPrefix ''Department

-- makeFields ''Department -- for _departmentNumber and _departmentName

-- class HasNumber s a | s -> a where
--   number :: Lens' s a
-- class HasName s a | s -> a where
--   name :: Lens' s a

-- instance HasNumber Department Int
-- instance HasName Department String


data Branch = Branch
  { ident    :: Int
  , code     :: String
  , location :: String
  } deriving (Show)

instance HasNumber Branch Int where
  number f (Branch i c l) = (\i' -> Branch i' c l) <$> f i

instance HasName Branch String where
  name f (Branch i c l) = (\c' -> Branch i c' l) <$> f c

modifyDepartment :: (HasName d String) => d -> d
modifyDepartment d = d & name .~ "Baz"

test1 :: IO ()
test1 = do
  let department1 = Department 6 "Foo"
  putStrLn $ "department1: " <> show department1
  print $ department1 ^. name
  print $ department1 & name .~ "Bar"
  print $ department1 & modifyDepartment

test2 :: IO ()
test2 = do
  let branch1 = Branch 6 "Foo" "Krakow"
  putStrLn $ "branch1: " <> show branch1
  print $ branch1 ^. name
  print $ branch1 & name .~ "Bar"
  print $ branch1 & modifyDepartment

run :: IO ()
run = sequence_ [ test1, test2 ]
