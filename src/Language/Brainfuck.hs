module Language.Brainfuck
    ( B.Brainfuck
    , B.inc
    , B.dec
    , B.fwd
    , B.bwd
    , B.printOut
    , B.readIn
    , B.while
    , B.showBrainfuck
    , B.readBrainfuck
    , runBrainfuck
    ) where

import qualified Language.Brainfuck.Brainfuck as B
import qualified Language.Brainfuck.BrainfuckVM as VM


runBrainfuck :: B.Brainfuck () -> IO ()
runBrainfuck = VM.runBrainfuckVM . VM.compileBrainfuck


