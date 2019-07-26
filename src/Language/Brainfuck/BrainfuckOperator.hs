module Language.Brainfuck.BrainfuckOperator
    ( BrainfuckOperator (..)
    , showBfops
    , readBfops
    ) where

import Control.Monad.State


data BrainfuckOperator
  = Inc
  | Dec
  | Fwd
  | Bwd
  | PrintOut
  | ReadIn
  | While [BrainfuckOperator]
  deriving
    ( Show
    , Eq
    )

showBfops :: [BrainfuckOperator] -> String
showBfops [] = ""
showBfops (Inc:xs) = '+': showBfops xs
showBfops (Dec:xs) = '-': showBfops xs
showBfops (Fwd:xs) = '>': showBfops xs
showBfops (Bwd:xs) = '<': showBfops xs
showBfops (PrintOut:xs) = '.': showBfops xs
showBfops (ReadIn:xs) = ',': showBfops xs
showBfops (While ops:xs) = '[': showBfops ops ++ "]" ++ showBfops xs

readBfops :: String -> [BrainfuckOperator]
readBfops = evalState (state r)
  where
    r :: String -> ([BrainfuckOperator], String)
    r [] = ([], "")
    r ('+':xs) = Inc +> r xs
    r ('-':xs) = Dec +> r xs
    r ('>':xs) = Fwd +> r xs
    r ('<':xs) = Bwd +> r xs
    r ('.':xs) = PrintOut +> r xs
    r (',':xs) = ReadIn +> r xs
    r ('[':xs) = let (ops, s) = r xs in While ops +> r s
    r (']':xs) = ([], xs)
    r (_:xs) = r xs
    op +> (ops, s) = (op:ops, s)

