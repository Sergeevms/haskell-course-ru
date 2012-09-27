{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.Primitive where

import Prelude (error,Show,Read)
import qualified Prelude as P
---------------------------------------------
-- Синтаксис лямбда-выражений

-- Эквивалентные определения
example1 x  = x
example1'   = \x -> x
example1''  = let y = \x -> x in y
example1''' = y where
    y = \x -> x

-- Снова эквивалентные определения
example2 x y  = x %+ y
example2' x   = \y -> x %+ y
example2''    = \x -> \y -> x %+ y
example2'''   = \x y -> x %+ y
example2''''  = let z = \x y -> x %+ y in z
example2''''' = z where
    z x = \y -> x %+ y

-- Зацикленное выражение
undefined = undefined

-- Ниже следует реализовать все термы, состоящие из undefined заглушки.
-- Любые термы можно переписывать (natEq и natLt --- хорошие кандидаты).

-------------------------------------------
-- Примитивные типы

-- Тип с единственным элементом
data Unit = Unit deriving (Show,Read)

-- Пара, произведение
data Pair a b = Pair { fst :: a, snd :: b } deriving (Show,Read)

-- Вариант, копроизведение
data Either a b = Left a | Right b deriving (Show,Read)

-- Частый частный случай, изоморфно Either Unit a
data Maybe a = Nothing | Just a deriving (Show,Read)

-- Частый частный случай, изоморфно Either Unit Unit
data Bool = False | True deriving (Show,Read)

-- Следует отметить, что встроенный if с этим Bool использовать нельзя,
-- зато case всегда работает.

-- Ну или можно реализовать свой if
if' True a b = a
if' False a b = b

-- Трихотомия. Замечательный тип, показывающий результат сравнения
data Tri = LT | EQ | GT deriving (Show,Read)

-------------------------------------------
-- Булевы значения

-- Логическое "НЕ"
not :: Bool -> Bool
not True = False
not False = True

infixr 3 &&
-- Логическое "И"
(&&) :: Bool -> Bool -> Bool
True  && x = x
False && _ = False

infixr 2 ||
-- Логическое "ИЛИ"
(||) :: Bool -> Bool -> Bool
True  || _ = True
False || x = x

-------------------------------------------
-- Натуральные числа

data Nat = Zero | Succ Nat deriving (Show,Read)

natZero = Zero     -- 0
natOne = Succ Zero -- 1

-- Сравнивает два натуральных числа
natCmp :: Nat -> Nat -> Tri
natCmp Zero Zero = EQ
natCmp Zero (Succ a) = LT
natCmp (Succ a) Zero = GT
natCmp (Succ a) (Succ b) = natCmp a b

-- n совпадает с m 
natEq :: Nat -> Nat -> Bool
natEq n m = case (natCmp n m) of
                EQ -> True
                _  -> False

-- n меньше m
natLt :: Nat -> Nat -> Bool
natLt n m = case (natCmp n m) of
                LT -> True
                _  -> False
infixl 6 +.
-- Сложение для натуральных чисел
(+.) :: Nat -> Nat -> Nat
Zero     +. m = m
(Succ n) +. m = Succ (n +. m)

infixl 6 -.
-- Вычитание для натуральных чисел
(-.) :: Nat -> Nat -> Nat
n -. Zero = n
(Succ n) -. (Succ m) = n -. m
Zero     -. (Succ m) = error"result isn't Natural number"

infixl 7 *.
-- Умножение для натуральных чисел
(*.) :: Nat -> Nat -> Nat
Zero     *. m = Zero
(Succ n) *. m = m +. (n *. m)

-- Целое и остаток от деления n на m
natDivMod :: Nat -> Nat -> Pair Nat Nat

natDivMod n m = case (natCmp n m) of
                LT -> Pair Zero n
                EQ -> Pair (Succ Zero) Zero
                GT -> Pair (Succ (fst new)) (snd new) where
                    new = natDivMod (n -. m) m

natDiv n = fst . natDivMod n
natMod n = snd . natDivMod n -- Остаток

-- Поиск GCD алгоритмом Евклида (должен занимать 2 (вычислителельная часть) + 1 (тип) строчки)
gcd :: Nat -> Nat -> Nat
gcd n Zero = n
gcd n m = gcd m (natMod n m)

-------------------------------------------
-- Целые числа

-- Требуется, чтобы представление каждого числа было единственным
data Int = Pos Nat | Neg Nat deriving (Show,Read)

intZero   = Pos Zero   -- 0
intOne    = Pos (Succ Zero)-- 1
intNegOne = Neg Zero-- -1

-- n -> - n
intNeg :: Int -> Int
intNeg (Pos Zero) = (Pos Zero)
intNeg (Pos n) = Neg (n -. natOne)
intNeg (Neg n) = Pos (n +. natOne)

-- Дальше также как для натуральных
intCmp :: Int -> Int -> Tri
intCmp (Neg n) (Pos m) = LT 
intCmp (Pos n) (Neg m) = GT 
intCmp (Pos n) (Pos m) = natCmp n m
intCmp (Neg n) (Neg m) = natCmp n m 

intEq :: Int -> Int -> Bool
intEq n m = case (intCmp n m) of
                EQ -> True
                _  -> False

intLt :: Int -> Int -> Bool
intLt n m = case (intCmp n m) of
                LT -> True
                _  -> False

infixl 6 .+., .-.
-- У меня это единственный страшный терм во всём файле
(.+.) :: Int -> Int -> Int
Pos n .+. Pos m = Pos (n +. m)
Neg n .+. Neg m = Neg (Succ (n +. m))
Pos n .+. Neg m = case (natCmp n x) of
                    EQ -> intZero
                    LT -> Neg (m -. n)
                    GT -> Pos (n -. x)
                    where x = Succ (m)
Neg n .+. Pos m = Pos m .+. Neg n

(.-.) :: Int -> Int -> Int
n .-. m = n .+. (intNeg m)

infixl 7 .*.
(.*.) :: Int -> Int -> Int
(Pos n) .*. (Pos m) = Pos (n *. m)
(Neg n) .*. (Neg m) = Pos ((Succ n) *. (Succ m))
(Neg n) .*. (Pos m) = intNeg (Pos ((Succ n) *. m))
(Pos n) .*. (Neg m) = (Neg m) .*. (Pos n)

-------------------------------------------
-- Рациональные числа

data Rat = Rat Int Nat

ratNorm :: Rat -> Rat
ratNorm (Rat (Pos (Zero)) _) = Rat (Pos (Zero)) natOne 
ratNorm (Rat _ Zero) = error"Division by Zero"
ratNorm (Rat (Pos n) m) = Rat (Pos (natDiv n g)) (natDiv m g) where g = gcd n m
ratNorm (Rat (Neg n) m) = Rat (Neg (natDiv (n +. natOne) g -. natOne)) (natDiv m g) where 
        g = gcd (n +. natOne) m

ratNeg :: Rat -> Rat
ratNeg (Rat x y) = Rat (intNeg x) y

-- У рациональных ещё есть обратные элементы
ratInv :: Rat -> Rat
ratInv (Rat (Pos n) m) = Rat (Pos m) n
ratInv (Rat (Neg n) m) = Rat (Neg (m -. natOne)) (n +. natOne)

-- Дальше как обычно
ratCmp :: Rat -> Rat -> Tri
ratCmp (Rat a1 b1) (Rat a2 b2) = intCmp (a1 .*. Pos b2) (a2 .*. Pos b1) 

ratEq :: Rat -> Rat -> Bool
ratEq a b = case (ratCmp a b) of
            EQ -> True
            _  -> False

ratLt :: Rat -> Rat -> Bool
ratLt a b = case (ratCmp a b) of
            LT -> True
            _  -> False

infixl 7 %+, %-
(%+) :: Rat -> Rat -> Rat
(Rat a b ) %+ (Rat c d ) = ratNorm (Rat x y) where
    x = (a .*. (Pos (d))) .+. (c .*. (Pos (b)))
    y = (b *. d)

(%-) :: Rat -> Rat -> Rat
n %- m = n %+ (ratNeg m)

infixl 7 %*, %/
(%*) :: Rat -> Rat -> Rat
(Rat a b) %* (Rat c d) = ratNorm (Rat (a .*. c) (b *. d))
(%/) :: Rat -> Rat -> Rat
n %/ m = n %* (ratInv m)

-- Перевод между новыми типами и стандартным Integer
natToInteger :: Nat -> P.Integer
natToInteger Zero = 0
natToInteger (Succ n) = 1 P.+ natToInteger n

integerToNat :: P.Integer -> Nat
integerToNat n | n P.== 0 = Zero
               | n P.> 0 = Succ (integerToNat (n P.- 1))
               | n P.< 0 = error "Input isn't Natural number"

intToInteger :: Int -> P.Integer
intToInteger (Pos Zero) = 0
intToInteger (Pos n) = natToInteger n
intToInteger (Neg n) = P.negate ((natToInteger n) P.+ 1)

integerToInt :: P.Integer -> Int
integerToInt n | n P.>= 0 = Pos (integerToNat n)
               | n P.< 0  = Neg (integerToNat (P.negate(n P.+ 1)))

ratToInteger :: Rat -> Pair P.Integer P.Integer
ratToInteger (Rat a b) = Pair (intToInteger a) (natToInteger b)

integerToRat :: P.Integer -> P.Integer -> Rat
integerToRat a b = ratNorm (Rat (integerToInt a) (integerToNat b))

-------------------------------------------
-- Операции над функциями.
-- Определены здесь, но использовать можно и выше

infixr 9 .
f . g = \ x -> f (g x)

infixr 0 $
f $ x = f x

-- Эквивалентные определения
example3   a b c = gcd a (gcd b c)
example3'  a b c = gcd a $ gcd b c
example3'' a b c = ($) (gcd a) (gcd b c)

-- И ещё эквивалентные определения
example4  a b x = (gcd a (gcd b x))
example4' a b = gcd a . gcd b
