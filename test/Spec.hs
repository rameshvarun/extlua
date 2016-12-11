import Test.Hspec

import Data.Text

import Language.ExtLua.Parser
import Language.ExtLua.Printer
import Language.ExtLua.Transform

main :: IO ()
main = hspec $ do
  describe "ExtLua Language Extensions" $ do
    it "Transforms statments like x += 1 into x = x + 1" $ do
      testTranspileFile "assignop.extlua" "assignop.lua"
    it "Transforms statments like $x and @y() into self.x and self:y()" $ do
      testTranspileFile "selfsugar.extlua" "selfsugar.lua"
    it "Transforms statments like x::print() into print(x)" $ do
      testTranspileFile "bindoperator.extlua" "bindoperator.lua"

testTranspileFile :: String -> String -> IO ()
testTranspileFile input expected = do
  expectedcode <- readFile ("./test/fixtures/" ++ expected)
  result <- parseFile ("./test/fixtures/" ++ input)
  case result of
    Right block -> shouldBe (strip $ pack $ show $ pprint $ transformBlock block) (strip $ pack $ expectedcode)
    Left _ -> expectationFailure ""
