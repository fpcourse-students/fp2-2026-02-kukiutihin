-- | Домашка 2. Параметрический полиморфизм: уровень 2.

-- Инстанс Functor для Vec из "Defs" объявляется здесь как задача 2.3, поэтому он orphan.
{-# OPTIONS_GHC -Wno-orphans #-}
module Level2 where

import Data.Kind (Type)
import Defs
import GHC.TypeLits (Symbol)
import MetaUtils (todo)


-- 2.1. Формулы: продвижение вручную
--
-- Поднимите на уровень типов синтаксис пропозициональных формул Prop вручную, без DataKinds
-- и TypeData: объявите пустые типы Var', Not' и типовые операторы ::\/, ::/\, ::->
-- (по одному на конструктор Prop). Пропозициональные переменные — пустые типы A и B.
-- Запишите типом PropExample формулу (a \/ b) -> (~a -> b) ровно с такой расстановкой
-- скобок; приоритеты своим операторам можно не назначать.

data A
data B

-- Здесь ваши объявления Var', Not', ::\/, ::/\ и ::->.

data Var' a 
data Not' a 

infixr 1 ::->
infixl 2 ::\/
infixl 3 ::/\

data (::->) a b
data (::\/) a b
data (::/\) a b

type PropExample = (Var' A ::\/ Var' B) ::-> (Not' (Var' A) ::-> Var' B)


-- 2.2. Формулы: DataKinds
--
-- Запишите ту же формулу типом PropDataExample, используя продвинутые конструкторы Prop
-- и строки уровня типов (кайнд Symbol, переменные "a" и "b"). Припишите PropDataExample
-- точный кайнд явно — сейчас в заглушке стоит неверный.

type PropDataExample :: Prop Symbol
type PropDataExample = (Var "a" :\/ Var "b") :-> (Not (Var "a") :-> (Var "b"))


-- 2.3. Функтор
--
-- Реализуйте Functor для вектора. Заметьте, что типы полностью обеспечивают выполнение
-- законов функтора.

instance Functor (Vec n) where
  _ `fmap` VNil = VNil
  f `fmap` (VCons el rest) = VCons (f el) (f <$> rest)


-- 2.4. Конкатенация
--
-- Реализуйте vconcat. Длину результата считает функция уровня типов NatPlus — закрытое
-- семейство типов: определение по образцу, как у обычной функции, только на типах.
-- Подробно семейства типов разбираются в главе 3, здесь достаточно прочитать определение.

type family NatPlus (n :: Nat) (m :: Nat) :: Nat where
  NatPlus Zero m = m
  NatPlus n Zero = n
  NatPlus (Suc n) m = Suc (NatPlus n m)

vconcat :: Vec n a -> Vec m a -> Vec (NatPlus n m) a
vconcat v1 VNil = v1
vconcat (VCons x xrest) v2@(VCons _ _) = VCons x (vconcat xrest v2)
vconcat VNil v2@(VCons _ _) = v2


-- 2.5. Гетерогенный zip
--
-- Реализуйте hzip двух гетерогенных списков. Список типов результата считает семейство Zip:
-- определите его по аналогии с NatPlus так, чтобы оно было определено на любых двух списках,
-- в том числе разной длины (лишний хвост отбрасывается, как у обычного zip).

type family Zip (as :: [Type]) (bs :: [Type]) :: [Type] where
  Zip '[] bs = '[]
  Zip as '[] = '[]
  Zip (x:xs) (y:ys) = (x, y) : Zip xs ys

hzip :: HList as -> HList bs -> HList (Zip as bs)
hzip HNil _ = HNil
hzip _ HNil = HNil
hzip (HCons x xrest) (HCons y yrest) = HCons (x, y) (hzip xrest yrest)


-- 2.6. Полиморфизм в кайндах
--
-- Tagged полиморфен по кайнду тега. Добейтесь Temperature :: TemperatureUnit -> Type -> Type,
-- не используя оператор (::) ни в определении Temperature, ни в его параметрах.
-- Подсказка: посмотрите на кайнд Tagged и вспомните, как передать тип явно.
-- Затем реализуйте c2f: перевод из Цельсия в Фаренгейт, f = c * 1.8 + 32.

newtype Tagged (tag :: k) (a :: Type) = MkTagged a

data TemperatureUnit = Celsius | Fahrenheit | Kelvin

type Temperature a = Tagged @TemperatureUnit a

c2f :: Temperature Celsius Double -> Temperature Fahrenheit Double
c2f (MkTagged num) = MkTagged $ num * 1.8 + 32


-- 2.7. Числа Чёрча в обёртке
--
-- Тип числа Чёрча полиморфен, поэтому функции над такими числами имеют высший ранг.
-- Обёртка Church прячет квантор под конструктор: снаружи все функции первого ранга,
-- но чтобы построить значение Church, под конструктором приходится написать полиморфную
-- функцию. Реализуйте zero, suc, plus, mult и fromInt (для неотрицательных чисел);
-- toInt дан для проверки.

newtype Church = Church (forall a. (a -> a) -> a -> a)

instance Show Church where 
  show (Church f) = show $ f (+ (1 :: Int)) 0
 
toInt :: Church -> Int
toInt (Church n) = n (+ 1) 0

zero :: Church
zero = Church \_ x -> x

suc :: Church -> Church
suc (Church f) = Church \g x -> g (f g x)

plus :: Church -> Church -> Church
plus (Church f) c2 = f suc c2

mult :: Church -> Church -> Church
mult (Church f) (Church g) = Church (f . g)

fromInt :: Int -> Church
fromInt i = Church \f x -> go f x i 
  where 
    go _ x 0 = x
    go f x i = f (go f x $ i - 1)



-- 2.8. Пара Чёрча
--
-- Pair — пара по Чёрчу, синоним с квантором внутри: функция, которая отдаёт обе компоненты
-- любому, кто скажет, что с ними делать. pair и pfst даны; реализуйте psnd и pswap.

type Pair a b = forall c. (a -> b -> c) -> c

pair :: a -> b -> Pair a b
pair x y f = f x y

pfst :: Pair a b -> a
pfst p = p const

psnd :: Pair a b -> b
psnd p = p (flip const)

pswap :: Pair a b -> Pair b a
pswap p = pair (psnd p) (pfst p)
