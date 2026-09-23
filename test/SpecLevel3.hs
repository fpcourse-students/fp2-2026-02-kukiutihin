{-# LANGUAGE TemplateHaskell #-}
module SpecLevel3 where

import Control.Exception (ErrorCall (..))
import Level3
import Test.Prelude
import TypeCheck

tests :: NamedTests
tests = nameTests 3
  [ testError
  ]

-- | Сигнатура сравнивается по форме (см. "TypeCheck"): имена переменных не важны,
-- @HasCallStack@ и другие контексты не учитываются.
testError :: Test
testError = TestList
  [ assertShapeMarked "3.1 сигнатура error'" (if actual == stub then "Todo" else actual)
      "(forall (v1 :: RuntimeRep) (v2 :: (TYPE v1)) (-> String v2))" actual
  , TestCase $ assertThrows @ErrorCall (error' "boom" :: Int)
  ]
  where
    actual = $(shapeOfType 'error')
    stub = "(forall (v1 :: Type) (-> String v1))" -- сигнатура выданной заглушки
