import TestParser

main :: IO ()
main =  do
          putStrLn "\n\n"
          putStrLn "[TEST SUITE]"
          putStrLn "[Parse Test]"
          _ <- mainParse
          putStrLn "[END OF TESTS]"


