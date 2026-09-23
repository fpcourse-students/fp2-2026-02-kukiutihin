module SpecLevel1 where

import Defs
import Level1
import Test.Prelude
import TypeCheck

tests :: NamedTests
tests = nameTests 1
  [ testTree
  , testVzip
  , testSnoc
  , testEval
  ]

testTree :: Test
testTree = typeIs @(Node' (Node' Leaf' Leaf') Leaf') @TreeExample "1.1"

v3 :: Vec (Suc (Suc (Suc Zero))) Int
v3 = VCons 1 (VCons 2 (VCons 3 VNil))

testVzip :: Test
testVzip = TestList
  [ TestCase $ assertEqual "vzip VNil VNil" ([] :: [(Int, Bool)]) $ vtoList (vzip VNil VNil)
  , TestCase $ assertEqual "vzip vexample vexample" [(1, 1), (2, 2)] $ vtoList (vzip vexample vexample)
  , TestCase $ assertEqual "vzip v3 (fmap show)" [(1, "1"), (2, "2"), (3, "3")] $
      vtoList (vzip v3 (VCons "1" (VCons "2" (VCons "3" VNil))))
  ]

testSnoc :: Test
testSnoc = TestList
  [ TestCase $ assertEqual "snoc VNil" [7 :: Int] $ vtoList (snoc VNil 7)
  , TestCase $ assertEqual "snoc vexample" [1, 2, 3] $ vtoList (snoc vexample 3)
  , TestCase $ assertEqual "snoc twice" [1, 2, 3, 4, 5] $ vtoList (snoc (snoc v3 4) 5)
  ]

testEval :: Test
testEval = TestList
  [ TestCase $ assertEqual "App" 42 $ eval (App (Const (+ 1)) (Const (41 :: Int)))
  , TestCase $ assertEqual "App twice" "ab" $
      eval (App (App (Const (++)) (Const "a")) (Const "b"))
  , TestCase $ assertEqual "MkPair" (1 :: Int, True) $ eval (MkPair (Const 1) (Const True))
  , TestCase $ assertEqual "Fst" 'x' $ eval (Fst (MkPair (Const 'x') (Const ())))
  , TestCase $ assertEqual "If IsZero Fst" "zero" $
      eval (If (IsZero (Fst (MkPair (Const 0) (Const 'y')))) (Const "zero") (Const "other"))
  , TestCase $ assertEqual "factorial 0" 1 $ factorial 0
  , TestCase $ assertEqual "factorial 5" 120 $ factorial 5
  , propertyToTest "factorial n" \(NonNegative n) ->
      n <= 12 ==> factorial n === product [1 .. n]
  ]
