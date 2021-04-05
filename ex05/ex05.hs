contaCh :: [Char] -> Int
contaCh [] = 0
contaCh (x:r) = 1 + contaCh r

conta :: [t] -> Int
conta [] = 0
conta (x:r) = 1 + conta r

maior :: [Int] -> Int
maior [x] = x
maior (x:y:r) | x > y = maior (x:r)
              | otherwise = maior (y:r)

primeiros :: Int -> [t] -> [t]
primeiros 0 _ = []
primeiros _ [] = []
primeiros n (x:xs) = x: primeiros (n-1) xs

-- achei isso no stackoverflow. Se não usasse esse "Eq t =>" não compilava com tipo t por causa do ==
pertence :: Eq t => t -> [t] -> Bool 
pertence _ [] = False
pertence x (y:z) | x == y = True
                 | otherwise = pertence x z

uniaoR :: Eq t => [t] -> [t] -> [t]
uniaoR [] (x:r) = x:r
uniaoR (x:r) [] = x:r
uniaoR lista (x:r) | pertence x lista == False = x: uniaoR lista r
                   | otherwise = uniaoR lista r

nPares :: [Int] -> Int
nPares [] = 0
nPares (x:r) | even x = 1 + nPares r
             | otherwise = nPares r

produto :: Num t => [t] -> t
produto [] = 1
produto (x:r) = x * produto r

tamanho :: [t] -> Int
tamanho [] = 0
tamanho (x:r) = 1 + tamanho r

comprime :: [[a]] -> [a]
comprime (x:[]) = x
comprime (x:y) = x ++ comprime y

uniaoAux ::Eq t => [t] -> [t] -> [t]
uniaoAux lista1 lista2 = [x | x <- lista2, (pertence x lista1) == False]

uniaoR2 :: Eq t => [t] -> [t] -> [t]
uniaoR2 [] (x:r) = x:r
uniaoR2 (x:r) [] = x:r
uniaoR2 lista1 lista2 = lista1 ++ uniaoAux lista1 lista2