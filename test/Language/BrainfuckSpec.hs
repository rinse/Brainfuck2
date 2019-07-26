module Language.BrainfuckSpec (spec) where

import Test.Hspec
import Language.Brainfuck

spec :: Spec
spec = do
  describe "showBrainfuck" $ do
    it "shows + when it gets inc" $ do
      showBrainfuck inc `shouldBe` "+"
    it "shows - when it gets dec" $ do
      showBrainfuck dec `shouldBe` "-"
    it "shows > when it gets fwd" $ do
      showBrainfuck fwd `shouldBe` ">"
    it "shows < when it gets bwd" $ do
      showBrainfuck bwd `shouldBe` "<"
    it "shows . when it gets printOut" $ do
      showBrainfuck printOut `shouldBe` "."
    it "shows , when it gets readIn" $ do
      showBrainfuck readIn `shouldBe` ","
    it "shows [] when it gets while" $ do
      showBrainfuck (while $ return ())`shouldBe` "[]"

  describe "readBrainfuck" $ do
    let normalize = showBrainfuck . readBrainfuck
    it "reads nothing then return nothing" $
      normalize "" `shouldBe` ""
    it "reads any other letters then it just ignore them" $
      normalize "any other letters are regarded as comment" `shouldBe` ""
    it "reads ops then return opts" $
      normalize "+-><.,[]" `shouldBe` "+-><.,[]"
    it "handles while properly" $
      normalize "+[+[--]]+" `shouldBe` "+[+[--]]+"

