type Data = (Int, Int, Int)
type Emprestimo = (String, String, Data, Data, String)
type Emprestimos = [Emprestimo]

valida :: Data -> Bool
valida (d, m,a) | (d>=1 && d<=31 && (m == 1 || m == 3 || m == 5 || m == 7 || m == 8 || m == 10 || m == 12)) = True
                | (d>=1 && d<=30 && (m == 4 || m == 6 || m == 9 || m == 11)) = True
                | (d>=1 && d<=28 && m == 2 && not(bissexto a)) = True
                | (d>=1 && d<=29 && m == 2 && bissexto a) = True
                | otherwise = False
    where
        bissexto ano | mod ano 400 == 0 = True
                     | (mod ano 4 == 0) && (mod ano 100 /= 0) = True
                     | otherwise  = False

bissextos :: [Int] -> [Int]
bissextos [] = []
bissextos (x:xs) | bissexto x = x:(bissextos xs)
                 | otherwise = bissextos xs
    where
        bissexto ano | mod ano 400 == 0 = True
                     | (mod ano 4 == 0) && (mod ano 100 /= 0) = True
                     | otherwise = False

atrasados ::Emprestimos-> Data -> Emprestimos
atrasados [] _ = []
atrasados (x:xs) dma | atrasado x dma = x:(atrasados xs dma)
                     | otherwise = atrasados xs dma
    where
        precede (d1,m1,a1) (d2,m2,a2) | a1 > a2 = False
                                      | a1 < a2 = True
                                      | m1 > m2 = False
                                      | m1 < m2 = True
                                      | d1 > d2 = False
                                      | d1 < d2 = True
                                      | otherwise = False
        atrasado (_, _, _, dDev, _) dAtual = not(precede dAtual dDev)

fiboAux :: Int -> (Int, Int)
fiboAux 0 = (0,1)
fiboAux n = passo (fiboAux (n-1))
    where
        passo (x,y) = (y, x+y)

fibo2 :: Int -> Int
fibo2 n = snd(fiboAux n)

fatorial :: Int -> Int
fatorial n = prodIntervalo 1 n
    where
        prodIntervalo m n
            | m >= n = n
            | otherwise = m * (prodIntervalo (m+1) n)

validaLet :: Data -> Bool
validaLet (d,m,a) =
    let
        bissexto ano | mod ano 400 == 0 = True
                     | (mod ano 4 == 0) && (mod ano 100 /= 0) = True
                     | otherwise  = False
        brac1 = (d>=1 && d<=31 && (m == 1 || m == 3 || m == 5 || m == 7 || m == 8 || m == 10 || m == 12))
        brac2 = (d>=1 && d<=30 && (m == 4 || m == 6 || m == 9 || m == 11))
        brac3 = (d>=1 && d<=28 && m == 2 && not(bissexto a))
        brac4 = (d>=1 && d<=29 && m == 2 && bissexto a)
    in
        brac1 || brac2 || brac3 || brac4

bissextosLet :: [Int] -> [Int]
bissextosLet l =
    let
        bissexto ano | mod ano 400 == 0 = True
                     | (mod ano 4 == 0) && (mod ano 100 /= 0) = True
                     | otherwise  = False
    in
        [x | x <- l, bissexto x]

atrasadosLet :: Emprestimos -> Data -> Emprestimos
atrasadosLet e dAtual =
    let
        precede (d1,m1,a1) (d2,m2,a2) | a1 > a2 = False
                                      | a1 < a2 = True
                                      | m1 > m2 = False
                                      | m1 < m2 = True
                                      | d1 > d2 = False
                                      | d1 < d2 = True
                                      | otherwise = False
        pegaEntrega (_, _, _, d, _) = d
    in
        [x | x <- e, precede(pegaEntrega x) dAtual]

fibo2Let :: Int -> Int
fibo2Let n =
    let
        passo (x,y) = (y, x+y)
        fibo2aux 0 = (0,1)
        fibo2aux x = passo(fibo2aux (x-1))
    in
        snd (fibo2aux n)

fatorialLet :: Int -> Int
fatorialLet n =
    let
        prodIntervalo a b
            | a >= b = b
            | otherwise = a*(prodIntervalo (a+1) b)
    in
        prodIntervalo 1 n


letA = (\x -> \y -> y)((\z -> z)(\z -> z))(\w -> w) 5
letB = ((\f -> (\x -> f(f x))) (\y -> y * y)) 3
letC = ((\f -> (\x -> f(f x))) (\y -> (+) y y)) 5
letD = ((\x -> (\y -> (+) x y) 5) ((\y -> (-) y 3) 7))
letE = (((\f -> (\x -> f(f(f x)))) (\y -> (y * y))) 2)
letF = (\x -> \y -> (+) x ((\x -> (-) x 3) y)) 5 6