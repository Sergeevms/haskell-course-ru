{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.List where

import Prelude (Show,Read,error)
import ITMOPrelude.Primitive

---------------------------------------------
-- Что надо делать?
--
-- Все undefined превратить в требуемые термы.
-- Звёздочкой (*) отмечены места, в которых может потребоваться думать.

---------------------------------------------
-- Определение

data List a = Nil |  Cons a (List a) deriving (Show,Read)

---------------------------------------------
-- Операции

-- Длина списка
length :: List a -> Nat
length Nil = Zero
length (Cons a ax) = Succ (length ax)

-- Склеить два списка за O(length a)
(++) :: List a -> List a -> List a
Nil ++ b = b
Cons a ax ++ b = Cons a (ax ++ b)

-- Список без первого элемента
tail :: List a -> List a
tail (Cons a ax) = ax
tail Nil = error "List is empty"

-- Список без последнего элемента
init :: List a -> List a
init (Cons a (Nil)) = Nil
init (Cons a ax) = Cons a (init ax) 
init Nil = error "List is empty"

-- Первый элемент
head :: List a -> a
head (Cons a (_)) = a
head Nil = error "List is empty"

-- Последний элемент
last :: List a -> a
last Nil = error "List is empty"
last (Cons a (Nil)) = a
last (Cons a ax) = last ax

-- n первых элементов списка
take :: Nat -> List a -> List a
take Zero _ = Nil
take n (Cons a ax) = Cons a (take (n -. natOne) ax)
take n Nil = error "List hasn't got so much elements"

-- Список без n первых элементов
drop :: Nat -> List a -> List a
drop n (Cons a ax) = drop (n -. natOne) ax
drop Zero a = a
drop n Nil = error "List hasn't got so much elements"

-- Оставить в списке только элементы удовлетворяющие p
filter :: (a -> Bool) -> List a -> List a
filter p (Cons a ax) = case (p a) of
                            True -> Cons a (filter p ax)
                            False -> filter p ax

-- Обобщённая версия. Вместо "выбросить/оставить" p
-- говорит "выбросить/оставить b".
gfilter :: (a -> Maybe b) -> List a -> List b
gfilter p Nil = Nil
gfilter p (Cons a ax) = case (p a) of
                             Nothing -> gfilter p ax
                             Just b  -> Cons b (gfilter p ax)

-- Копировать из списка в результат до первого нарушения предиката
-- takeWhile (< 3) [1,2,3,4,1,2,3,4] == [1,2]
takeWhile :: (a -> Bool) -> List a -> List a
takeWhile p Nil = Nil
takeWhile p (Cons a ax) = case (p a) of
                               True -> Cons a (takeWhile p ax)
                               False -> Nil

-- Не копировать из списка в результат до первого нарушения предиката,
-- после чего скопировать все элементы, включая первый нарушивший
-- dropWhile (< 3) [1,2,3,4,1,2,3,4] == [3,4,1,2,3,4]
dropWhile :: (a -> Bool) -> List a -> List a
dropWhile p Nil = Nil
dropWhile p (Cons a ax) = case (p a) of
                               True -> dropWhile p ax
                               False -> ax
-- Разбить список по предикату на (takeWhile p xs, dropWhile p xs),
-- но эффективнее
span :: (a -> Bool) -> List a -> Pair (List a) (List a)
span _ xs @ Nil = Pair xs xs
span p xs @ (Cons x xs') = case (p x) of
                                  True -> let Pair ys zs = span p xs' in Pair (Cons x ys) zs
                                  False -> Pair Nil xs

-- Разбить список по предикату на (takeWhile (not . p) xs, dropWhile (not . p) xs),
-- но эффективнее
break :: (a -> Bool) -> List a -> Pair (List a) (List a)
break _ xs @ Nil = Pair xs xs
break p xs @ (Cons x xs') = case (p x) of
                                   True -> Pair Nil xs
                                   False -> let Pair ys zs = break p xs' in Pair (Cons x ys) zs
-- n-ый элемент списка (считая с нуля)
(!!) :: List a -> Nat -> a
Nil !! n = error "!!: empty list"
(Cons a ax) !! Zero = a
(Cons a ax) !! n = ax !! (n -. natOne)

-- Список задом на перёд
reverse :: List a -> List a
reverse a = rev a Nil
    where
        rev Nil a = a
        rev (Cons x xs) a = rev xs (Cons x a)

-- (*) Все подсписки данного списка
subsequences :: List a -> List (List a)
subsequences a = Cons Nil (nonEmptSubs a)

nonEmptSubs :: List a -> List (List a)
nonEmptSubs Nil = Nil
nonEmptSubs (Cons a as) = Cons (Cons a Nil) (foldr f Nil (nonEmptSubs as))
                              where f bs r = Cons bs (Cons (Cons a bs) r)

id :: a -> a
id a = a

-- (*) Все перестановки элементов данного списка
permutations :: List a -> List (List a)
permutations a = Cons a (perm a Nil)
    where
        perm Nil _ = Nil
        perm (Cons t ts) is = foldr interleave (perm ts (Cons t is)) (permutations is)
            where interleave    xs          r = let Pair _ zs = interleave' id xs r in zs
                  interleave' _ Nil         r = Pair ts r
                  interleave' f (Cons y ys) r = let Pair us zs = interleave' (f . (Cons y)) ys r
                                                in  Pair (Cons y us) (Cons (f (Cons t (Cons y us))) zs)

repeat :: a -> List a
repeat x = xs where xs = Cons x xs

foldl :: (a -> b -> a) -> a -> List b -> a
foldl f a0 xs0 = lgo a0 xs0
    where
        lgo z Nil = z
        lgo z (Cons x xs) = lgo (f z x) xs

scanl :: (a -> b -> a) -> a -> List b -> List a
scanl f q ls = Cons q (case ls of
                        Nil -> Nil
                        Cons x xs -> scanl f (f q x) xs)

foldr :: (a -> b -> b) -> b -> List a -> b
foldr f z0 xs0 = rgo z0 xs0
    where
        rgo z Nil = z
        rgo z (Cons x xs) = f x (rgo z xs)

scanr :: (a -> b -> b) -> b -> List a -> List b
scanr _ b Nil = Cons b Nil
scanr f b0 (Cons a as) = Cons (f a b) bs
                              where bs@(Cons b _) = scanr f b0 as

-- Должно завершаться за конечное время
finiteTimeTest = take (Succ $ Succ $ Succ $ Succ Zero) $ foldr (Cons) Nil $ repeat Zero

-- Применяет f к каждому элементу списка
map :: (a -> b) -> List a -> List b
map f Nil = Nil
map f (Cons a ax) = Cons (f a) (map f ax)

-- Склеивает список списков в список
concat :: List (List a) -> List a
concat = foldr (++) Nil

-- Эквивалент (concat . map), но эффективнее
concatMap :: (a -> List b) -> List a -> List b
concatMap f = foldr ((++) . f) Nil

-- Сплющить два списка в список пар длинны min (length a, length b)
zip :: List a -> List b -> List (Pair a b)
zip (Cons a ax) (Cons b bx) = Cons (Pair a b) (zip ax bx)

-- Аналогично, но плющить при помощи функции, а не конструктором Pair
zipWith :: (a -> b -> c) -> List a -> List b -> List c
zipWith f (Cons a as) (Cons b bs) = Cons (f a b) (zipWith f as bs)
