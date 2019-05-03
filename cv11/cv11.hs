main :: IO()
main = do putStr "Enter a string: \n"
          xs <- getLine
          putStr "This string has "
          putStr (show (length xs))
          putStr " characters.\n"
