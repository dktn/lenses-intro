{-# LANGUAGE TemplateHaskell #-}

module Example5 where

import Control.Lens

data Person =
  Person
    { _firstName  :: String
    , _lastName   :: String
    , _department :: Department
    } deriving (Show)

data Department =
  Department
    { _number :: Int
    , _name   :: String
    } deriving (Show)

firstName :: Lens' Person String
firstName f (Person fn ln d) = (\fn' -> Person fn' ln d) <$> f fn

-- type Lens' s a = Lens s s a a
-- type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t
-- firstName :: Functor f => (a -> f a) -> (Person a) -> f (Person a)

test1 :: IO ()
test1 = do
  let person1 = Person "Adam" "Szlachta" $ Department 6 "Foo"
  putStrLn $ "person1: " <> show person1
  print $ person1 ^. firstName
  pure ()

run :: IO ()
run = sequence_ [ test1 ]
