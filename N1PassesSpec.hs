module Main where

import Test.Hspec
import Data.Int
import CompilerPasses
import N1Passes
import N1

uniquifyResult = getResult . uniquify
rcoResult prog = getResult (passRemoveComplexOperas (CState 0 (Right prog)))

spec :: Spec
spec = do
  describe "Uniqueify Tests:" $ do
    it "can uniqueify a simple let expression" $ do
      uniquifyResult (Program (Let "x" (Int 5) (Var "x"))) `shouldBe`
        Right (Program (Let "s0" (Int 5) (Var "s0")))

    it "can uniqueify a simple nested let expression" $ do
      uniquifyResult (Program (Let "x" (Int 5) (Let "y" (Int 5) (Var "x"))))  `shouldBe`
        Right (Program (Let "s0" (Int 5) (Let "s1" (Int 5) (Var "s0"))))

    it "can uniqueify a simple nested let expression" $ do
      uniquifyResult (Program (Let "x" (Int 5) (Let "y" (Int 5) (Var "y"))))  `shouldBe`
        Right (Program (Let "s0" (Int 5) (Let "s1" (Int 5) (Var "s1"))))

    it "can uniqueify a shadowed name nested let expression" $ do
      uniquifyResult (Program (Let "x" (Int 5) (Let "x" (Int 5) (Var "x"))))  `shouldBe`
        Right (Program (Let "s0" (Int 5) (Let "s1" (Int 5) (Var "s1"))))

    it "can uniqueify a nested let expression under Add" $ do
      uniquifyResult (Program (Let "x" (Int 5) (Add (Int 6) (Let "y" (Int 5) (Var "y")))))  `shouldBe`
        Right (Program (Let "s0" (Int 5) (Add (Int 6) (Let "s1" (Int 5) (Var "s1")))))

    it "can uniqueify a nested let expression after recurssion" $ do
      uniquifyResult (Program (Let "x" (Int 5) (Add (Var "x") (Let "y" (Int 5) (Var "y")))))  `shouldBe`
        Right (Program (Let "s0" (Int 5) (Add (Var "s0") (Let "s1" (Int 5) (Var "s1")))))

    it "can uniqueify a nested let expression after recurssion" $ do
      uniquifyResult (Program (Let "x" (Int 5) (Add (Let "y" (Int 5) (Var "y")) (Var "x"))))  `shouldBe`
        Right (Program (Let "s0" (Int 5) (Add (Let "s1" (Int 5) (Var "s1")) (Var "s0"))))

    it "can return an Left correctly when the variable is out of scope" $ do
      uniquifyResult (Program (Let "x" (Var "z") (Add (Var "x") (Let "y" (Int 5) (Var "y")))))  `shouldBe`
        Left "Symbol 'z' not found"

    it "can return an Left correctly when the variable is out of scope" $ do
      uniquifyResult (Program (Let "x" (Var "x") (Add (Int 6) (Let "x" (Int 5) (Var "x")))))  `shouldBe`
        Left "Symbol 'x' not found"

    -- ============================================
    -- ADDITIONAL UNIQUIFY TESTS (Required by Rubric)
    -- ============================================
    
    it "can uniqueify triple-nested shadowing" $ do
      uniquifyResult (Program (Let "x" (Int 5) (Let "x" (Int 6) (Let "x" (Int 7) (Var "x"))))) `shouldBe`
        Right (Program (Let "s0" (Int 5) (Let "s1" (Int 6) (Let "s2" (Int 7) (Var "s2")))))
    
    it "can handle shadowing with multiple vars in scope" $ do
      uniquifyResult (Program (Let "x" (Int 1) (Let "y" (Int 2) (Let "x" (Int 3) (Add (Var "x") (Var "y")))))) `shouldBe`
        Right (Program (Let "s0" (Int 1) (Let "s1" (Int 2) (Let "s2" (Int 3) (Add (Var "s2") (Var "s1"))))))
    
    it "detects unbound variable deep in nested lets" $ do
      uniquifyResult (Program (Let "x" (Int 5) (Let "y" (Add (Var "x") (Var "z")) (Var "y")))) `shouldBe`
        Left "Symbol 'z' not found"

  describe "Remove complex opera* pass tests:" $ do
    it "can rco on Ints" $ do
      rcoResult (Program (Int 7)) `shouldBe` Right (Program (Int 7))
    it "can rco on Read" $ do
      rcoResult (Program Read) `shouldBe` Right (Program Read)
    it "can rco on Var" $ do
      rcoResult (Program (Var "x")) `shouldBe` Right (Program (Var "x"))
    it "can rco on Negate Var" $ do
      rcoResult (Program (Negate (Var "x"))) `shouldBe` Right (Program (Negate (Var "x")))
    it "can rco on Negate Int" $ do
      rcoResult (Program (Negate (Int 6))) `shouldBe` Right (Program (Negate (Int 6)))
    it "can rco on Negate Negate Int" $ do
      rcoResult (Program (Negate (Negate (Int 6)))) `shouldBe` Right (Program (Let "s0" (Negate (Int 6)) (Negate (Var "s0"))))
    it "can rco on Negate Negate Var" $ do
      rcoResult (Program (Negate (Negate (Var "x")))) `shouldBe` Right (Program (Let "s0" (Negate (Var "x")) (Negate (Var "s0"))))
    it "can rco on Add Int Int" $ do
      rcoResult (Program (Add (Int 6) (Int 7))) `shouldBe` Right (Program (Add (Int 6) (Int 7)))
    it "can rco on Add Var Int" $ do
      rcoResult (Program (Add (Var "x") (Int 7))) `shouldBe` Right (Program (Add (Var "x") (Int 7)))
    it "can rco on Add Var (Negate Int)" $ do
      rcoResult (Program (Add (Var "x") (Negate (Int 7)))) `shouldBe` Right (Program (Let "s0" (Negate (Int 7)) (Add (Var "x") (Var "s0"))))
    it "can pass case on Add (Negate (Var \"x\")) (Negate (Int 7)))" $ do
      rcoResult (Program (Add (Negate (Var "x")) (Negate (Int 7)))) `shouldBe`
        Right (Program
                (Let "s0" (Negate (Var "x"))
                  (Let "s1" (Negate (Int 7))
                    (Add (Var "s0") (Var "s1")))))
    it "can rco pass case on: (Program (Add (Negate (Int 5)) (Add (Negate (Int 7)) (Negate (Int 8)))))" $ do
      rcoResult (Program (Add (Negate (Int 5)) (Add (Negate (Int 7)) (Negate (Int 8))))) `shouldBe`
        Right (Program (Let "s0" (Negate (Int 5))
                          (Let "s1" (Negate (Int 7))
                             (Let "s2" (Negate (Int 8))
                                (Let "s3" (Add (Var "s1") (Var "s2"))
                                  (Add (Var "s0") (Var "s3")))))))
    it "can rco on let x = 5 in x" $ do
      rcoResult (Program (Let "x" (Int 5) (Var "x"))) `shouldBe`
        Right (Program (Let "x" (Int 5) (Var "x")))
    it "can rco on let x = -5 in x" $ do
      rcoResult (Program (Let "x" (Negate (Int 5)) (Var "x"))) `shouldBe`
        Right (Program (Let "x" (Negate (Int 5)) (Var "x")))
    it "can rco on let x = -5+3 in x" $ do
      rcoResult (Program (Let "x" (Add (Negate (Int 5)) (Int 3)) (Var "x"))) `shouldBe`
        Right (Program (Let "x" (Let "s0" (Negate (Int 5)) (Add (Var "s0") (Int 3))) (Var "x")))
    it "can rco on let x = 5 in -(x + x)" $ do
      rcoResult (Program (Let "x" (Int 5) (Negate (Add (Var "x") (Var "x"))))) `shouldBe`
        Right (Program (Let "x" (Int 5) (Let "s0" (Add (Var "x") (Var "x")) (Negate (Var "s0")))))
    it "can rco on let x = 5 in -(x + -x)" $ do
      rcoResult (Program (Let "x" (Int 5) (Negate (Add (Var "x") (Negate (Var "x")))))) `shouldBe`
        Right (Program (Let "x" (Int 5) (Let "s0" (Negate (Var "x")) (Let "s1" (Add (Var "x") (Var "s0")) (Negate (Var "s1"))))))

    -- ============================================
    -- ADDITIONAL RCO TESTS (Required by Rubric)
    -- ============================================
    
    it "can rco on deeply nested Add" $ do
      rcoResult (Program (Add (Add (Int 1) (Negate (Int 2))) (Int 3))) `shouldBe`
        Right (Program (Let "s0" (Negate (Int 2)) (Let "s1" (Add (Int 1) (Var "s0")) (Add (Var "s1") (Int 3)))))
    
    it "can rco on let with complex binding and body" $ do
      rcoResult (Program (Let "x" (Add (Int 1) (Negate (Int 2))) (Add (Var "x") (Negate (Var "x"))))) `shouldBe`
        Right (Program (Let "x" (Let "s0" (Negate (Int 2)) (Add (Int 1) (Var "s0"))) (Let "s1" (Negate (Var "x")) (Add (Var "x") (Var "s1")))))
    
    it "can rco with Read in complex position" $ do
      rcoResult (Program (Add (Negate Read) (Int 5))) `shouldBe`
        Right (Program (Let "s0" (Negate Read) (Add (Var "s0") (Int 5))))

main :: IO ()
main = hspec spec