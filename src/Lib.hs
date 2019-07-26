module Lib
    ( someFunc
    ) where

import Control.Monad
import Control.Monad.State
import Language.Brainfuck

helloWorld :: Brainfuck ()
helloWorld = do
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
  fwd >> fwd>>  printOut
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

someFunc :: IO ()
someFunc = do
  runBrainfuck $ readBrainfuck "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."
  runBrainfuck helloWorld
  print $ showBrainfuck helloWorld

