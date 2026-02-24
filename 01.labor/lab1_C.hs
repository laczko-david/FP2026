{- I. Könyvtárfüggvények használata nélkül,
definiáljuk azt a függvényt, amely meghatározza -}

-- - két szám összegét, különbségét, szorzatát, hányadosát, osztási maradékát,
osszeg :: (Num a) => a -> a -> a
osszeg a b = a + b

osszeg2 :: Int -> Int -> Int
osszeg2 a b = (+) a b

kulonbseg a b = a - b

kulonbseg2 :: Double -> Double -> Double
kulonbseg2 a b = (-) a b

szorzat a b = a * b

szorzat2 a b = (*) a b

hanyados :: (Fractional a) => a -> a -> a
hanyados a b = a / b

hanyados2 :: (Integral a) => a -> a -> a
hanyados2 a b = div a b

hanyados3 :: (Integral a) => a -> a -> a
hanyados3 a b = a `div` b

osztmar :: (Integral a) => a -> a -> a
osztmar a b = mod a b

osztmar2 :: (Integral a) => a -> a -> a
osztmar2 a b = a `mod` b

-- - egy első fokú egyenlet gyökét,
-- a*x + b = 0 -> a, b -> x = -b / a
elsoF a b = -b / a

-- - egy szám abszulút értékét,
absz n = if n < 0 then -n else n

absz2 n
  | n < 0 = -n
  | otherwise = n

-- - egy szám előjelét,
elojel n = if n < 0 then "neg" else if n > 0 then "poz" else "nulla"

elojel2 n
  | n < 0 = "neg"
  | n > 0 = "poz"
  | otherwise = "nulla"

-- - két argumentuma közül a maximumot,
max1 a b
  | a > b = a
  | otherwise = b

max2 a b = if a > b then a else b

-- - két argumentuma közül a minimumot,
min1 a b
  | a < b = a
  | otherwise = b

min2 a b = if a < b then a else b

-- - egy másodfokú egyenlet gyökeit,
-- a*(x**2) + b*x + c = 0 -> a b c
-- delta = b**2 - 4*a*c
-- gy1 = (-b - sqrt delta) / (2*a)
-- gy2 = (-b + sqrt delta) / (2*a)
masodF a b c
  | delta < 0 = error "komplex szamok"
  | delta == 0 = [gy1]
  | otherwise = [gy1, gy2]
  where
    delta = b ** 2 - 4 * a * c
    gy1 = (-b - sqrt delta) / (2 * a)
    gy2 = (-b + sqrt delta) / (2 * a)

masodF2 a b c
  | delta < 0 = error "komplex szamok"
  | otherwise = (gy1, gy2)
  where
    delta = b ** 2 - 4 * a * c
    gy1 = (-b - sqrt delta) / (2 * a)
    gy2 = (-b + sqrt delta) / (2 * a)

masodF3 a b c = if delta < 0 then error "komplex szamok" else (gy1, gy2)
  where
    delta = b ** 2 - 4 * a * c
    gy1 = (-b - sqrt delta) / (2 * a)
    gy2 = (-b + sqrt delta) / (2 * a)

-- - hogy két elempár értékei "majdnem" megegyeznek-e: akkor térít vissza True értéket a függvény, ha a két pár ugyanazokat az értékeket tartalmazza függetlenül az elemek sorrendjétől.
--   Például: $$(6, 7)$$ egyenlő $$(7,6)$$-al, de $$(6, 7)$$ nem egyenlő $$(4, 7)$$-el.
elempar (a, b) (c, d) = if (a == c && b == d) || (a == d && b == c) then True else False

elempar2 (a, b) (c, d) = (a == c && b == d) || (a == d && b == c)

elempar3 ep1 ep2 = (a == c && b == d) || (a == d && b == c)
  where
    (a, b) = ep1
    (c, d) = ep2

-- - az n szám faktoriálisát (3 módszer),
fakt1 0 = 1
fakt1 n = n * fakt1 (n - 1)

fakt2 n
  | n < 0 = -1
  | n == 0 = 1
  | otherwise = n * fakt2 (n - 1)

-- meghivas: fakt3 1 5
fakt3 :: Int -> Int -> Int
fakt3 res n
  | res < 0 || n < 0 = error "negativ bemenet"
  | n == 0 = res
  | otherwise = fakt3 (n * res) (n - 1)

-- - az x szám n-ik hatványát, ha a kitevő pozitív szám (3 módszer).
hatvanyXN x n
  | n < 0 = error "neg kitevo"
  | otherwise = x ** n

hatvanyXN2 x n
  | n < 0 = error "neg kitevo"
  | otherwise = x ^ n

hatvanyXN3 x n
  | n < 0 = error "neg kitevo"
  | n == 0 = 1
  | otherwise = x * hatvanyXN3 x (n - 1)

-- II. Könyvtárfüggvények használata nélkül, illetve halmazkifejezéseket alkalmazva, definiáljuk azt a függvényt, amely meghatározza:

-- - az első n természetes szám negyzetgyökét,
negyzetgyok n = [sqrt i | i <- [1 .. n]]

-- - az első n négyzetszámot,
negyzet n = [i ^ 2 | i <- [1 .. n]]

-- - az első n természetes szám köbét,
kob n = [i ^ 3 | i <- [1 .. n]]

-- - az első n olyan természetes számot, amelyben nem szerepelnek a négyzetszámok,
nemNegyzet n = [i | i <- [0 .. n], (sqrt i ^ 2) /= i]

-- - x hatványait adott n-ig,
hatvany x n = [x ^ i | i <- [0 .. n]]

-- - egy szám páros osztóinak listáját,
parosOsztok x = [i | i <- [1 .. x], mod x i == 0, mod i 2 == 0]

-- - n-ig a prímszámok listáját,
osztok x = [i | i <- [1 .. x], mod x i == 0]

primszam x = osztok x == [1, x]

primszamokN n = [i | i <- [2 .. n], primszam i]

primszamokN2 n = [i | i <- [2 .. n], primszamL i]
  where
    primszamL ns = osztokL ns == [1, ns]
    osztokL ns2 = [i | i <- [1 .. ns2], mod ns2 i == 0]

-- - n-ig az összetett számok listáját,
osszetett n = [i | i <- [1 .. n], primszam i == False]

-- - n-ig a páratlan összetett számok listáját,
paratlanOsszetett n = [i | i <- [1 .. n], not (primszam i), mod i 2 /= 0]

paratlanOsszetett2 n = [i | i <- [1, 3 .. n], not (primszam i)]

-- - az n-nél kisebb Pitágorászi számhármasokat,
pitagorasz n = [(a, b, c) | c <- [1 .. n], b <- [1 .. c], a <- [1 .. b], a ** 2 + b ** 2 == c ** 2]

-- - a következő listát: $$[(\texttt{a},0), (\texttt{b},1),\ldots, (\texttt{z}, 25)]$$,
betuSzam = zip ['a' .. 'z'] [0 .. 25]

-- - a következő listát: $$[(0, 5), (1, 4), (2, 3), (3, 2), (4, 1), (5, 0)]$$, majd általánosítsuk a feladatot.
szamok = zip [0 .. 5] [5, 4 .. 0]

szamok1 n = zip [0 .. n] [n, n - 1 .. 0]

szamok2 n = [(i, n - i) | i <- [0 .. n]]

-- - azt a listát, ami felváltva tartalmaz True és False értékeket.
tfLs n = take n ls
  where
    ls = [True, False] ++ ls

tfLs2 n = [even i | i <- [0 .. n]]