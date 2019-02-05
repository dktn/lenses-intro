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

department :: Lens' Person Department
department f (Person fn ln d) = (\d' -> Person fn ln d') <$> f d

name :: Lens' Department String
name f (Department nr nm) = (\nm' -> Department nr nm') <$> f nm

-- type Lens' s a = Lens s s a a
-- type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t
-- firstName :: Functor f => (a -> f a) -> (Person a) -> f (Person a) -- consider to use in libs

test1 :: IO ()
test1 = do
  let person1 = Person "Adam" "Szlachta" $ Department 6 "Foo"
  putStrLn $ "person1: " <> show person1
  print $ person1 ^. firstName
  print $ person1 ^. department . name
  print $ person1 & department . name .~ "Bar"
  pure ()

data Weather =
  Weather
    { _windSpeed :: Double
    , _temperature :: Double
    } deriving (Show)

makeLenses ''Weather

temperatureFahrenheit :: Lens' Weather Double
temperatureFahrenheit f (Weather w tC) = (\tF -> Weather w $ toCelsius tF) <$> (f $ toFahrenheit tC)
  where
    toFahrenheit tC =  tC * 9 / 5 + 32
    toCelsius tF = (tF - 32) * 5 / 9

test2 :: IO ()
test2 = do
  let weather1 = Weather 3.5 0.0
  putStrLn $ "weather1: " <> show weather1
  print $ weather1 & windSpeed *~ 2
  print $ weather1 ^. temperature
  print $ weather1 ^. temperatureFahrenheit
  let weather2 = weather1 & temperatureFahrenheit -~ 32
  print $ weather2 ^. temperature
  print $ weather2 ^. temperatureFahrenheit

run :: IO ()
run = sequence_ [ test1, test2 ]
