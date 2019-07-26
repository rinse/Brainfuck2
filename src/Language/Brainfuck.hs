module Language.Brainfuck
    ( Brainfuck
    , runBrainfuck
    , execBrainfuck
    , inc
    , dec
    , fwd
    , bwd
    , printOut
    , readIn
    , while
    , showBrainfuck
    , readBrainfuck
    ) where

import Control.Monad.Writer
import Language.BrainfuckOperator


type Brainfuck a = Writer [BrainfuckOperator] a

runBrainfuck :: Brainfuck a -> (a, [BrainfuckOperator])
runBrainfuck = runWriter

execBrainfuck :: Brainfuck a -> [BrainfuckOperator]
execBrainfuck = execWriter

putOp :: BrainfuckOperator -> Brainfuck ()
putOp op = tell [op]

inc, dec, fwd, bwd, printOut, readIn :: Brainfuck ()
inc = putOp Inc
dec = putOp Dec
fwd = putOp Fwd
bwd = putOp Bwd
printOut = putOp PrintOut
readIn = putOp ReadIn

while :: Brainfuck () -> Brainfuck ()
while bfs = putOp $ While (execWriter bfs)

showBrainfuck :: Brainfuck () -> String
showBrainfuck = showBfops . execWriter

readBrainfuck :: String -> Brainfuck ()
readBrainfuck s = writer ((), readBfops s)

