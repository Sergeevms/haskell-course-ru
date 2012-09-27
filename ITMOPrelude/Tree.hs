{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.Tree where

import ITMOPrelude.Primitive
import Prelude (Show,Read,error)
import ITMOPrelude.List

data Tree a = Empt | Node {left :: Tree a, value :: a, right :: Tree a} deriving (Show,Read)

-- Создание пустого дерева
createEmptyTree :: a -> Tree a
createEmptyTree _ = Empt

-- Переход в левого потомка
moveLeftTree :: Tree a -> Tree a
moveLeftTree Empt = Empt
moveLeftTree (Node l _ _) = l

-- Переход в правого потомка
moveRightTree :: Tree a -> Tree a
moveRightTree Empt = Empt
moveRightTree (Node _ _ r) = r

-- Создание элемента дерева
createElement :: a -> Tree a
createElement a = Node Empt a Empt

-- Добавить элемент в текущую позицию, оставив старое дерево левым потомком
addTreeElementL :: Tree a -> a -> Tree a
addTreeElementL Empt         a = createElement a
addTreeElementL (Node l b r) a = Node (Node l b r) a Empt

-- То же самое, только для правого потомка
addTreeElementR :: Tree a -> a -> Tree a
addTreeElementR Empt         a = createElement a
addTreeElementR (Node l b r) a = Node Empt a (Node l b r)

-- Добавить элемент самым левым
addTreeElementML :: Tree a -> a -> Tree a
addTreeElementML (Node Empt b r) a = Node (createElement a) b r
addTreeElementML Empt                a = createElement a
addTreeElementML ax                  a = addTreeElementML (moveLeftTree ax) a

-- Добавить элемент самым правым
addTreeElementMR :: Tree a -> a -> Tree a
addTreeElementMR Empt                a = createElement a
addTreeElementMR (Node l b Empt) a = Node l b (createElement a)
addTreeElementMR ax                  a = addTreeElementMR (moveRightTree ax) a

-- Изменение значения, хранящегося в вершине
editTreeElementValue :: Tree a -> a -> Tree a
editTreeElementValue Empt            a = createElement a
editTreeElementValue (Node l b r) a = Node l a r

-- Левый поворот
leftRotateTree :: Tree a -> Tree a
leftRotateTree (Node p a (Node q b r)) = Node (Node p a q) b r
leftRotateTree _ = error "Couldn't perform left rotate on that tree"

-- Правый поворот
rightRotateTree :: Tree a -> Tree a
rightRotateTree (Node (Node p a q) b r) = Node p a (Node q b r)
rightRotateTree _ = error "Couldn't perform right rotate on that tree"

-- Аналог map
mapTree :: (a -> b) -> Tree a -> Tree b
mapTree _ Empt = Empt
mapTree f (Node l a r) =  Node (mapTree f l) (f a) (mapTree f r)

-- Аналог foldr
foldrTree :: (a -> b -> b) -> b -> Tree a -> b
foldrTree _ b Empt = b
foldrTree f b (Node l a r) = foldrTree f (f a (foldrTree f b r)) l 
