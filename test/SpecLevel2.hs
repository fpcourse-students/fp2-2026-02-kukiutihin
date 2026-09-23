{-# LANGUAGE TemplateHaskell #-}
module SpecLevel2 where

import Defs
import Level2
import Test.Prelude
import TypeCheck

tests :: NamedTests
tests = nameTests 2
  [ testPropManual
  , testPropDataKinds
  , testFunctor
  , testVconcat
  , testHzip
  , testTemperature
  , testChurch
  , testPair
  ]

-- | Формулы 2.1 и 2.2 сравниваются по форме дерева типа (см. "TypeCheck"): имена операторов
-- и переменных должны совпадать с условием, расстановка скобок — тоже.
testPropManual :: Test
testPropManual = assertShape "2.1"
  "(::-> (::\\/ (Var' A) (Var' B)) (::-> (Not' (Var' A)) (Var' B)))"
  $(synonymShape ''PropExample)

testPropDataKinds :: Test
testPropDataKinds = TestList
  [ typeIs @(('Var "a" ':\/ 'Var "b") ':-> ('Not ('Var "a") ':-> 'Var "b")) @PropDataExample "2.2"
  , assertShapeMarked "2.2 кайнд" $(synonymShape ''PropDataExample) "(Prop Symbol)" $(shapeOfKind ''PropDataExample)
  ]

v3 :: Vec (Suc (Suc (Suc Zero))) Int
v3 = VCons 1 (VCons 2 (VCons 3 VNil))

testFunctor :: Test
testFunctor = TestList
  [ TestCase $ assertEqual "fmap VNil" ([] :: [Int]) $ vtoList (fmap (+ 1) VNil)
  , TestCase $ assertEqual "fmap vexample" [2, 3] $ vtoList (fmap (+ 1) vexample)
  , TestCase $ assertEqual "fmap show v3" ["1", "2", "3"] $ vtoList (fmap show v3)
  ]

testVconcat :: Test
testVconcat = TestList
  [ TestCase $ assertEqual "VNil ++ VNil" ([] :: [Int]) $ vtoList (vconcat VNil VNil)
  , TestCase $ assertEqual "VNil ++ v3" [1, 2, 3] $ vtoList (vconcat VNil v3)
  , TestCase $ assertEqual "v3 ++ VNil" [1, 2, 3] $ vtoList (vconcat v3 VNil)
  , TestCase $ assertEqual "vexample ++ v3" [1, 2, 1, 2, 3] $ vtoList (vconcat vexample v3)
  ]

testHzip :: Test
testHzip = TestList
  [ TestCase $ assertEqual "length: equal" 3 $ hlength (hzip hexample hexample)
  , TestCase $ assertEqual "length: shorter left" 1 $ hlength (hzip (HCons 'x' HNil) hexample)
  , TestCase $ assertEqual "length: shorter right" 0 $ hlength (hzip hexample HNil)
  , TestCase $ assertEqual "contents" "HCons (42,'x') (HCons (True,\"y\") HNil)" $
      show (hzip hexample (HCons 'x' (HCons "y" HNil)))
  ]

-- | Проверяется только кайнд: в интерфейсе GHC @Tagged \@TemperatureUnit@ и
-- @(Tagged :: TemperatureUnit -> Type -> Type)@ выглядят одинаково, так что требование
-- «без @::@» из условия тестом не проверить.
testTemperature :: Test
testTemperature = TestList
  [ assertShapeMarked "2.6 кайнд Temperature" marker "(-> TemperatureUnit (-> Type Type))" kind
  , TestCase $ assertEqual "c2f 100" 212 $ unTagged (c2f (MkTagged 100))
  , propertyToTest "c2f" \c -> unTagged (c2f (MkTagged c)) === c * 1.8 + 32
  ]
  where
    unTagged :: Tagged tag a -> a
    unTagged (MkTagged x) = x
    -- Выданная заглушка — сам Tagged с полиморфным кайндом; ответ Tagged @TemperatureUnit
    -- рендерится так же, но его кайнд уже без forall.
    kind = $(shapeOfKind ''Temperature)
    marker = if $(synonymShape ''Temperature) == "Tagged" && take 7 kind == "(forall" then "Todo" else kind

testChurch :: Test
testChurch = TestList
  [ TestCase $ assertEqual "zero" 0 $ toInt zero
  , TestCase $ assertEqual "suc (suc zero)" 2 $ toInt (suc (suc zero))
  , propertyToTest "fromInt" \(NonNegative n) -> toInt (fromInt n) === n
  , propertyToTest "plus" \(NonNegative n, NonNegative m) ->
      toInt (plus (fromInt n) (fromInt m)) === n + m
  , propertyToTest "mult" \(NonNegative n, NonNegative m) ->
      n <= 100 && m <= 100 ==> toInt (mult (fromInt n) (fromInt m)) === n * m
  ]

testPair :: Test
testPair = TestList
  [ TestCase $ assertEqual "psnd" 'x' $ psnd (pair (1 :: Int) 'x')
  , TestCase $ assertEqual "pfst . pswap" 'x' $ pfst (pswap (pair (1 :: Int) 'x'))
  , TestCase $ assertEqual "psnd . pswap" (1 :: Int) $ psnd (pswap (pair (1 :: Int) 'x'))
  , propertyToTest "pswap . pswap" \(x :: Int, y :: Bool) ->
      unpair (pswap (pswap (pair x y))) === (x, y)
  ]
  where
    unpair :: Pair a b -> (a, b)
    unpair p = (pfst p, psnd p)
