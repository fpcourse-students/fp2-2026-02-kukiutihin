{- |
Проверки задач уровня типов, которые не ломают сборку тестов при неверном ответе
и не печатают ожидаемый ответ.

* 'typeIs' — сравнение через @Typeable@: оба типа превращаются в 'SomeTypeRep' во время
  выполнения, так что неверный тип — обычный проваленный тест, а не ошибка компиляции.
* 'shapeOfType', 'shapeOfKind', 'synonymShape' — форма типа или кайнда по Template Haskell:
  дерево TH рендерится в строку с именами без модулей, переменные под @forall@ нумеруются
  по порядку связывания, контексты и скобки не учитываются. Это единственный способ
  посмотреть на кайнд синонима или на сигнатуру с @forall@, не заставляя GHC её проверять.
-}
module TypeCheck
  ( typeIs
  , shapeOfType, shapeOfKind, synonymShape
  , assertShape, assertShapeMarked, isTodo
  ) where

import Control.Monad (when)
import Data.List qualified as List
import Language.Haskell.TH
import Test.HUnit (Test (..), assertBool)
import TodoException (todo)
import Type.Reflection (SomeTypeRep (..), Typeable, typeRep)

import Defs (Todo)

-- | Тип @actual@ (ответ студента) совпадает с @expected@. Заглушка 'Todo' — задача не начата.
typeIs :: forall {k1} {k2} (expected :: k1) (actual :: k2). (Typeable expected, Typeable actual) => String -> Test
typeIs what = TestCase do
  let actual = SomeTypeRep (typeRep @actual)
  when (actual == SomeTypeRep (typeRep @Todo)) $ todo what
  assertBool (what <> ": тип " <> show actual <> " не тот, что требуется") $
    actual == SomeTypeRep (typeRep @expected)

-- | Сплайс: форма типа именованной сущности (для функции — её сигнатура) строкой.
shapeOfType :: Name -> Q Exp
shapeOfType name = reifyType name >>= stringE . render

-- | Сплайс: форма кайнда типа (для синонима — его кайнд целиком, включая параметры).
shapeOfKind :: Name -> Q Exp
shapeOfKind = shapeOfType

-- | Сплайс: форма правой части синонима типа.
synonymShape :: Name -> Q Exp
synonymShape name = reify name >>= \case
  TyConI (TySynD _ _ rhs) -> stringE $ render rhs
  _ -> stringE "<not a type synonym>"

-- | Заглушка уровня типов из "Defs".
isTodo :: String -> Bool
isTodo = (== "Todo")

-- | Форма совпадает с ожидаемой; заглушка — задача не начата.
assertShape :: String -> String -> String -> Test
assertShape what expected actual = assertShapeMarked what actual expected actual

-- | То же, но «начата ли задача» определяется по отдельной форме-маркеру (например, по правой
-- части синонима, когда сравнивается его кайнд).
assertShapeMarked :: String -> String -> String -> String -> Test
assertShapeMarked what marker expected actual = TestCase do
  when (isTodo marker) $ todo what
  assertBool (what <> ": форма " <> actual <> " не та, что требуется") $ actual == expected

-- | Рендер дерева типа: аппликация — @(f x y)@, стрелка — @(-> a b)@, квантор —
-- @(forall (v1 :: k) … t)@; имена без модулей, продвинутость конструкторов и явные
-- аппликации кайндов не различаются, @[Char]@ печатается как @String@.
render :: Type -> String
render = go []
  where
    go env = \case
      ForallT binders _ body ->
        let names = map binderName binders
            env' = env ++ names
            bound = zipWith (binderShape env') binders [length env + 1 ..]
        in "(forall " <> unwords bound <> " " <> go env' body <> ")"
      ForallVisT binders body -> go env $ ForallT (map (SpecifiedSpec <$) binders) [] body
      AppT ListT (ConT c) | nameBase c == "Char" -> "String"
      t@AppT {} -> let (h, args) = spine t in
        "(" <> unwords (go env h : map (go env) args) <> ")"
      AppKindT t _ -> go env t
      SigT t _ -> go env t
      ParensT t -> go env t
      InfixT l n r -> go env $ AppT (AppT (ConT n) l) r
      UInfixT l n r -> go env $ AppT (AppT (ConT n) l) r
      PromotedInfixT l n r -> go env $ AppT (AppT (ConT n) l) r
      PromotedUInfixT l n r -> go env $ AppT (AppT (ConT n) l) r
      VarT n -> variable env n
      ConT n -> nameBase n
      PromotedT n -> nameBase n
      TupleT 0 -> "()"
      TupleT n -> "(" <> replicate (n - 1) ',' <> ")"
      PromotedTupleT n -> "(" <> replicate (n - 1) ',' <> ")"
      ArrowT -> "->"
      MulArrowT -> "->"
      ListT -> "[]"
      PromotedNilT -> "[]"
      PromotedConsT -> ":"
      StarT -> "Type"
      ConstraintT -> "Constraint"
      LitT (NumTyLit n) -> show n
      LitT (StrTyLit s) -> show s
      LitT (CharTyLit c) -> show c
      EqualityT -> "~"
      WildCardT -> "_"
      ImplicitParamT n t -> "(?" <> n <> " " <> go env t <> ")"
      UnboxedTupleT n -> "(#" <> replicate (n - 1) ',' <> "#)"
      UnboxedSumT n -> "(#" <> replicate (n - 1) '|' <> "#)"
    binderName = \case
      PlainTV n _ -> n
      KindedTV n _ _ -> n
    binderShape env b i = case b of
      PlainTV {} -> "v" <> show i
      KindedTV _ _ k -> "(v" <> show i <> " :: " <> go env k <> ")"
    variable env n = case List.elemIndex n env of
      Just i -> "v" <> show (i + 1)
      Nothing -> nameBase n

-- | Голова аппликации и её аргументы: @f x y@ → @(f, [x, y])@.
spine :: Type -> (Type, [Type])
spine = \case
  AppT f x -> let (h, args) = spine f in (h, args ++ [x])
  t -> (t, [])
