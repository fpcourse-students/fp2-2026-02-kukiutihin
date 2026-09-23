{- |
Выданный код домашки 2: типы, общие для нескольких уровней. Редактировать не нужно,
решения пишутся в "Level1", "Level2" и "Level3".
-}
module Defs where

import Data.Kind (Type)

-- | Заглушка для задач уровня типов — аналог @todo@ для термов: «здесь должен быть ваш тип».
-- Замените её ответом. Пока на месте ответа стоит 'Todo', тесты показывают задачу как TODO.
data Todo

-- * Числа и векторы

-- | Натуральные числа Пеано. С DataKinds конструкторы 'Zero' и 'Suc' можно использовать
-- как типы кайнда @Nat@.
data Nat = Zero | Suc Nat

-- | Вектор: список, длина которого записана в типе.
data Vec (size :: Nat) (elem :: Type) where
  VNil :: Vec Zero a
  VCons :: a -> Vec n a -> Vec (Suc n) a

vexample :: Vec (Suc (Suc Zero)) Int
vexample = VCons 1 (VCons 2 VNil)

-- | Элементы вектора списком. Тип результата не зависит от длины, поэтому функция
-- одинаково подходит для векторов любой длины — тестам она нужна именно поэтому.
vtoList :: Vec n a -> [a]
vtoList = \case
  VNil -> []
  VCons x xs -> x : vtoList xs

-- * Гетерогенный список

-- | Список, индексированный списком типов своих элементов.
data HList (tys :: [Type]) where
  HNil :: HList '[]
  HCons :: t -> HList ts -> HList (t ': ts)

hexample :: HList '[Int, Bool, Double]
hexample = HCons 42 $ HCons True $ HCons 12.5 HNil

-- | Число элементов; не зависит от типов элементов.
hlength :: HList ts -> Int
hlength = \case
  HNil -> 0
  HCons _ xs -> 1 + hlength xs

instance Show (HList '[]) where
  show HNil = "HNil"

instance (Show t, Show (HList ts)) => Show (HList (t ': ts)) where
  showsPrec p (HCons x xs) = showParen (p > 10) $
    showString "HCons " . showsPrec 11 x . showString " " . showsPrec 11 xs

-- * Пропозициональные формулы

-- | Синтаксис формул с переменными типа @var@. Приоритеты — как у @&&@, @||@ и импликации:
-- @a :\\/ b :-> Not (Var a) :-> Var b@ читается как @(a :\\/ b) :-> (Not (Var a) :-> Var b)@.
data Prop var
  = Var var
  | Not (Prop var)
  | Prop var :\/ Prop var
  | Prop var :/\ Prop var
  | Prop var :-> Prop var

infixr 1 :->
infixl 2 :\/
infixl 3 :/\
