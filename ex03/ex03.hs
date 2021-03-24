myOr1 :: Bool -> Bool -> Bool
myOr1 True True = True
myOr1 True False = True
myOr1 False True = True
myOr1 False False = False

myOr2 :: Bool -> Bool -> Bool
myOr2 False False = False
myOr2 _ _ = True

myOr3 :: Bool -> Bool -> Bool
myOr3 False b = b
myOr3 True _ = True

myOr4 :: Bool -> Bool -> Bool
myOr4 x y = if(x == y)
                then x
                else True

myOr5 :: Bool -> Bool -> Bool
myOr5 x y = if(x == False)
                then y
                else True

type Ponto3D = (Float, Float, Float)
distP3 :: Ponto3D -> Ponto3D -> Float
distP3 (x1,y1,z1) (x2,y2,z2) = sqrt((x1-x2)^2 + (y1-y2)^2 + (z1-z2)^2)

fatorialg :: Int -> Int
fatorialg n | n == 0 = 1
            | otherwise  = n*fatorialg(n-1)

fatorialp :: Int -> Int
fatorialp 0 = 1
fatorialp n = n*fatorialp(n-1)

fibog :: Int -> Int
fibog n | n == 1 = 1
        | n == 2 = 1
        | otherwise = fibog(n-2) + fibog(n-1)

fibop :: Int -> Int
fibop 1 = 1
fibop 2 = 1
fibop n = fibop(n-2)+fibop(n-1)

nTri :: Int -> Int
nTri 1 = 1
nTri n = n + nTri(n-1)

pow2 :: Int -> Int
pow2 0 = 1
pow2 n = 2*pow2 (n-1)

prodIntervalo :: Int -> Int -> Int
prodIntervalo m n | m == n = n
                  | otherwise = m*(prodIntervalo (m+1) n)

fatorialPI :: Int -> Int
fatorialPI n = prodIntervalo 1 n

restoDiv :: Int -> Int -> Int
restoDiv m n | m < n = m
             | otherwise = restoDiv (m-n) n

divInteira :: Int -> Int -> Int
divInteira m n | m < n = 0
               | otherwise = 1 + divInteira(m-n) n

mdcg :: (Int, Int) -> Int
mdcg (m,n) | n == 0 = m
           | otherwise = mdcg(n, mod m n)

mdcp :: (Int, Int) -> Int
mdcp (m,0) = m
mdcp (m,n) = mdcp(n, mod m n)

binomial :: (Int, Int) -> Int
binomial (n, 0) = 1
binomial (n,k) = if(k == n)
        then 1
        else binomial(n - 1, k) + binomial(n-1,k-1)

passo :: (Int, Int) -> (Int, Int)
passo (x, y) = (y, x+y)

fiboAux :: Int -> (Int, Int)
fiboAux 0 = (0,1)
fiboAux n = passo (fiboAux (n-1))

fibo2 :: Int -> Int
fibo2 n = snd(fiboAux n)