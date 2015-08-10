module Main where

add :: Int -> Int -> Int
add x y = x + y

square :: Int -> Int
square x = x * x

add_cps :: Int -> Int -> (Int -> r) -> r
add_cps x y = \k -> k (add x y)

square_cps :: Int -> (Int -> r) -> r
square_cps x = \k -> k (square x)

pythagoras_cps :: Int -> Int -> (Int -> r) -> r
pythagoras_cps x y = \k ->
  square_cps x $ \x_sqrd ->
  square_cps y $ \y_sqrd ->
    add_cps x_sqrd y_sqrd k

chainCPS :: ((a -> r) -> r) -> (a -> (b -> r) -> r) -> (b -> r) -> r
chainCPS s f = \k -> s $ \x -> f x k
