module Main where 



type Setter a b = b -> a -> a 

first :: Setter (a, b) a 
first x (_a, b) = (x, b)



main :: IO ()
main = putStrLn "Hello lawal"


-- play.haskell.org