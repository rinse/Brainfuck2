module Main
    ( main
    ) where

import Control.Monad (replicateM_)
import Language.Brainfuck


-- from [https://en.wikipedia.org/wiki/Brainfuck]
helloWorld :: String
helloWorld = "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."


-- equivalent to the above
helloWorld' :: Brainfuck ()
helloWorld' = do
  replicateM_ 8 inc
  while $ do
    fwd >> replicateM_ 4 inc
    while $ do
      fwd >> replicateM_ 2 inc
      fwd >> replicateM_ 3 inc
      fwd >> replicateM_ 3 inc
      fwd >> inc
      replicateM_ 4 bwd
      dec
    fwd >> inc
    fwd >> inc
    fwd >> dec
    fwd >> fwd >> inc
    while $ do
      bwd
    bwd >> dec
  fwd >> fwd >> printOut
  fwd >> replicateM_ 3 dec >> printOut
  replicateM_ 7 inc >> printOut
  printOut
  replicateM_ 3 inc >> printOut
  fwd >> fwd >> printOut
  bwd >> dec >> printOut
  bwd >> printOut
  replicateM_ 3 inc >> printOut
  replicateM_ 6 dec >> printOut
  replicateM_ 8 dec >> printOut
  fwd >> fwd >> inc >> printOut
  fwd >> replicateM_ 2 inc >> printOut


main :: IO ()
main = do
  runBrainfuck . readBrainfuck $ helloWorld
  runBrainfuck helloWorld'
  print $ showBrainfuck helloWorld'
  print $ showBrainfuck . readBrainfuck $ helloWorld

