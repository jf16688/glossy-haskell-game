module Codeword where
import System.IO.Unsafe


dictionary :: [String]
dictionary = unsafePerformIO $ lines <$> readFile "src/dictionary.txt"