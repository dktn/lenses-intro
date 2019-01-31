-- Compare with Example2

module Example1 where



data Point a =
  Point
    { x :: a
    , y :: a
    } deriving (Show)



data Line a =
  Line
    { lineStart :: Point a
    , lineEnd   :: Point a
    } deriving (Show)



makePoint :: (a, a) -> Point a
makePoint (x, y) = Point x y

makeLine :: (a, a) -> (a, a) -> Line a
makeLine start end = Line (makePoint start) (makePoint end)

run :: IO ()
run = do
  putStrLn "Example 1 - start"

  -- access nested property
  let line1 = makeLine (1, 2) (3, 4)
  putStrLn $ "line1: " <> show line1
  putStrLn $ "line1.lineEnd.x: " <> show (x . lineEnd $ line1)

  -- modify top level property
  let line2 = line1 { lineStart = makePoint (0, 1) }
  putStrLn $ "line2: " <> show line2
  -- create without using accessors
  let line3 = Line (Point 1 3) (Point 3 4)
  putStrLn $ "line3: " <> show line3
  -- create using accessors
  let line4 = Line { lineStart = Point { x = 1, y = 2 }, lineEnd = Point { x = 3, y = 4 } }
  putStrLn $ "line4: " <> show line4
  let line5 =
        Line
          { lineStart = Point { x = 1, y = 2 }
          , lineEnd   = Point { x = 3, y = 4 }
          }
  putStrLn $ "line5: " <> show line5

  -- multiply endline.x by 2
  let lineEnd1 = lineEnd line1
      line6 = line1 { lineEnd = lineEnd1 { y = 2 * y lineEnd1 } }
  putStrLn $ "line6: " <> show line6

  -- multiply endline.x by 2, no helper variable
  let line7 = line1 { lineEnd = (lineEnd line1) { y = 2 * y (lineEnd line1) } }
  putStrLn $ "line7: " <> show line7

  putStrLn "Example 1 - end"
