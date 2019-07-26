module Language.Brainfuck.BrainfuckOperatorSpec (spec) where

import Test.Hspec
import Language.Brainfuck.BrainfuckOperator

spec :: Spec
spec = do
  describe "showBfops" $ do
    it "shows + when it gets Inc" $ do
      showBfops [Inc] `shouldBe` "+"
    it "shows - when it gets Dec" $ do
      showBfops [Dec] `shouldBe` "-"
    it "shows > when it gets fwd" $ do
      showBfops [Fwd] `shouldBe` ">"
    it "shows < when it gets bwd" $ do
      showBfops [Bwd] `shouldBe` "<"
    it "shows . when it gets printOut" $ do
      showBfops [PrintOut] `shouldBe` "."
    it "shows , when it gets readIn" $ do
      showBfops [ReadIn] `shouldBe` ","
    it "shows [] when it gets while" $ do
      showBfops [While []]`shouldBe` "[]"

  describe "readBfops" $ do
    it "reads + then returns Inc" $
      readBfops "+" `shouldBe` [Inc]
    it "reads - then returns Dec" $
      readBfops "-" `shouldBe` [Dec]
    it "reads > then returns Fwd" $
      readBfops ">" `shouldBe` [Fwd]
    it "reads < then returns Bwd" $
      readBfops "<" `shouldBe` [Bwd]
    it "reads . then returns PrintOut" $
      readBfops "." `shouldBe` [PrintOut]
    it "reads , then returns ReadIn" $
      readBfops "," `shouldBe` [ReadIn]
    it "reads [] then returns While []" $
      readBfops "[]" `shouldBe` [While []]
    it "interprets nested while properly" $
      readBfops "[[]]" `shouldBe` [While [While []]]
    it "reads nothing then returns nothing" $
      readBfops "" `shouldBe` []
    it "ignores letters" $
      readBfops "hello world !" `shouldBe` []

