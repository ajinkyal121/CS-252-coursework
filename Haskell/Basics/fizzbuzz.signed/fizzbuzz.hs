-- Do the game fizzbuzz (http://en.wikipedia.org/wiki/Fizz_buzz).
-- Return a string counting from 1 to the specified number.
-- Replace numbers dvisible by 3 with "fizz" and numbers divisible
-- by 5 with "buzz".  If a number is divisible by both 3 and 5,
-- replace it with "fizzbuzz".

fizzbuzz :: Int -> String
fizzbuzz 1 = "1"
fizzbuzz 0 = ""

fizzbuzz x 
                  | x < 0 = error "Non-negative numbers only" 
                  | x `mod` 15 == 0 = fizzbuzz (x-1) ++ " FizzBuzz"
                  | x `mod` 3 == 0 = fizzbuzz (x-1) ++ " Fizz"
                  | x `mod` 5 == 0 = fizzbuzz (x-1) ++ " Buzz"
                  | otherwise = fizzbuzz (x-1) ++ " " ++ show x



main :: IO ()
main = do
  print (fizzbuzz 1)
  print (fizzbuzz 7)
  print $ fizzbuzz 99
  print $ fizzbuzz 0
  print $ fizzbuzz (-2)
