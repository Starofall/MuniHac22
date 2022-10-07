module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn $ show $ add4 9

add4 :: Int -> Int
add4 = (+4)
