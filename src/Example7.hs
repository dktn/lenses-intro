{-# LANGUAGE TemplateHaskell #-}

module Example7 where

import Control.Lens

data Department =
  Department
    { _number :: Int
    , _name   :: String
    } deriving (Show)

-- makeLenses ''Department
-- number :: Lens' Department Int
-- number :: Functor f => (Int -> f Int) -> Department -> f Department
-- number f (Department a b) = (\a' -> Department a' b) <$> f a

makeClassy ''Department

-- number :: (HasDepartment c, Functor f) => (Int -> f Int) -> c -> f c

-- class HasDepartment t where
--   department :: Lens' t Department
--   number :: Lens' t Int
--   number = department . go where go f (Department x y) = (\x' -> Department x' y) <$> f x
--   name :: Lens' t String
--   name = department . go where go f (Department x y) = (\y' -> Department x y') <$> f y
-- instance HasDepartment Department where
--   department = id

data Branch = Branch
  { ident    :: Int
  , code     :: String
  , location :: String
  } deriving (Show)

instance HasDepartment Branch where
  department f (Branch i c l) = (\(Department i' c') -> Branch i' c' l) <$> f (Department i c)
  number     f (Branch i c l) = (\i' -> Branch i' c l) <$> f i
  name       f (Branch i c l) = (\c' -> Branch i c' l) <$> f c

modifyDepartment :: (HasDepartment d) => d -> d
modifyDepartment d = d & name .~ "Baz"

test1 :: IO ()
test1 = do
  let department1 = Department 6 "Foo"
  putStrLn $ "department1: " <> show department1
  print $ department1 ^. name
  print $ department1 & name .~ "Bar"
  print $ department1 & modifyDepartment
  print $ department1 ^. department

test2 :: IO ()
test2 = do
  let branch1 = Branch 6 "Foo" "Krakow"
  putStrLn $ "branch1: " <> show branch1
  print $ branch1 ^. name
  print $ branch1 & name .~ "Bar"
  print $ branch1 & modifyDepartment
  print $ branch1 ^. department

run :: IO ()
run = sequence_ [ test1, test2 ]
