module Language.Brainfuck.BrainfuckVM
    ( BrainfuckVM
    , compileBrainfuck
    , runBrainfuckVM
    ) where

import Control.Monad
import Control.Monad.State
import Data.Char (chr, ord)
import Data.Zipper
import Language.Brainfuck.Brainfuck
import Language.Brainfuck.BrainfuckOperator


getCur :: Zipper a -> a
getCur (Zipper _ a _) = a
modifyCur :: (a -> a) -> Zipper a -> Zipper a
modifyCur f (Zipper l c r) = Zipper l (f c) r
setCur :: a -> Zipper a -> Zipper a
setCur = modifyCur . const

type Memory = Zipper Int

emptyMemory :: Memory
emptyMemory = Zipper (repeat 0) 0 (repeat 0)

type BrainfuckVM a = StateT Memory IO a

inc', dec', fwd', bwd', printOut', readIn' :: BrainfuckVM ()
inc' = modify' $ modifyCur (+ 1)
dec' = modify' $ modifyCur (subtract 1)
fwd' = modify' left
bwd' = modify' right
printOut' = get >>= lift . (putChar . chr) . getCur
readIn' = lift getChar >>= modify . setCur . ord

while' :: BrainfuckVM () -> BrainfuckVM ()
while' vm = do
  m <- get
  when (getCur m /= 0) $ do
    vm
    while' vm


compileBfops :: [BrainfuckOperator] -> BrainfuckVM ()
compileBfops [] = return ()
compileBfops (Inc:xs) = inc' >> compileBfops xs
compileBfops (Dec:xs) = dec' >> compileBfops xs
compileBfops (Fwd:xs) = fwd' >> compileBfops xs
compileBfops (Bwd:xs) = bwd' >> compileBfops xs
compileBfops (PrintOut:xs) = printOut' >> compileBfops xs
compileBfops (ReadIn:xs) = readIn' >> compileBfops xs
compileBfops (While ops:xs) = while' (compileBfops ops) >> compileBfops xs

compileBrainfuck :: Brainfuck () -> BrainfuckVM ()
compileBrainfuck = compileBfops . execBrainfuck

runBrainfuckVM :: BrainfuckVM () -> IO ()
runBrainfuckVM = flip evalStateT emptyMemory

