-- | Домашка 2. Параметрический полиморфизм: уровень 1, обязательные задачи.
--
-- Выданные типы ('Vec', 'HList', 'Prop', заглушка 'Todo') лежат в "Defs".
-- Решения пишутся на месте заглушек: @todo@ в термах, 'Todo' в типах.
module Level1 where

import Defs
import MetaUtils (todo)


-- 1.1. Дерево на уровне типов
--
-- Дано дерево Tree. Оно уже поднято на уровень типов вручную, без DataKinds: пустые типы
-- Leaf' и Node' повторяют форму конструкторов. Запишите типом TreeExample дерево
-- `Node (Node Leaf Leaf) Leaf`.

data Tree = Leaf | Node Tree Tree

data Leaf'
data Node' left right

type TreeExample = Todo


-- 1.2. Безопасный zip
--
-- Реализуйте vzip двух векторов одной длины. Заметьте, что эту функцию невозможно
-- реализовать неправильно: любая ошибка не пройдёт проверку типов.

vzip :: Vec n a -> Vec n b -> Vec n (a, b)
vzip = todo "1.2"


-- 1.3. Добавление в конец
--
-- Реализуйте snoc — добавление элемента в конец вектора. Заметьте, что эту функцию уже
-- можно реализовать неправильно. Двигайтесь последовательно: поставьте на место результата
-- дыру `_` и читайте, какой тип и какие равенства GHC от вас ожидает в каждой ветке.

snoc :: Vec n a -> a -> Vec (Suc n) a
snoc = todo "1.3"


-- 1.4. Типизированный интерпретатор
--
-- Expr — синтаксис встроенного языка; тип выражения индексирован типом Haskell, в который
-- оно вычисляется. Допишите ветки eval для аппликации App и пар MkPair, Fst.
-- Затем реализуйте factorial программой на этом языке: конструкции Haskell и встроенного
-- языка можно смешивать, а вычисляет программу eval.

data Expr ty where
  Const :: ty -> Expr ty
  IsZero :: Expr Int -> Expr Bool
  If :: Expr Bool -> Expr ty -> Expr ty -> Expr ty
  App :: Expr (arg -> res) -> Expr arg -> Expr res
  MkPair :: Expr a -> Expr b -> Expr (a, b)
  Fst :: Expr (a, b) -> Expr a

eval :: Expr ty -> ty
eval = \case
  Const x -> x
  IsZero e -> eval e == 0
  If c t e -> if eval c then eval t else eval e
  App _ _ -> todo "1.4 eval App"
  MkPair _ _ -> todo "1.4 eval MkPair"
  Fst _ -> todo "1.4 eval Fst"

factorial :: Int -> Int
factorial = todo "1.4 factorial"
