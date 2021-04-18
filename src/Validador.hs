module Validador where

import Data.Char

validarPrimeiroDigito :: String -> Bool
validarPrimeiroDigito s = validarDigito s 10
--validarPrimeiroDigito s = undefined

validarSegundoDigito :: String -> Bool
validarSegundoDigito s = validarDigito s 11
--validarSegundoDigito s = undefined

validarDigito :: String -> Int -> Bool 
validarDigito s d = if d >= 10 && d <= 11 then
                        sum (valores (zip (digitos (take (d-1) s)) [d, (d-1)..2])) * 10 `mod` 11 == digitToInt (s !! (d-1))
                        --normalizar (sum (valores (zip (digitos (take (d-1) s)) [d, (d-1)..2])) * 10 `mod` 11) == digitToInt (s !! (d-1))
                    else 
                        False

-- normalizar :: Int -> Int 
-- normalizar i = if i == 10 then 0 else i

digitos :: String -> [Int]
digitos = map digitToInt

valores :: [(Int,Int)] -> [Int]
valores = map valor

valido :: String -> Bool
valido s = validarPrimeiroDigito s && validarSegundoDigito s

valor :: (Int,Int) -> Int 
valor (x, y) = x * y