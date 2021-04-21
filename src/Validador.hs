module Validador where

import Data.Char

validarPrimeiroDigito :: String -> Bool
validarPrimeiroDigito s = validarDigito s 10

validarSegundoDigito :: String -> Bool
validarSegundoDigito s = validarDigito s 11

validarDigito :: String -> Int -> Bool 
validarDigito s d
    | d >= 10 && d <= 11 = normalizar (sum (zipWith (*) (digitos (take (d-1) s)) [d, (d-1)..2]) * 10 `mod` 11) == digitToInt (s !! (d-1))
    | otherwise = False

normalizar :: Int -> Int
normalizar = flip mod 10

digitos :: String -> [Int]
digitos = map digitToInt

valido :: String -> Bool
valido s = validarPrimeiroDigito s && validarSegundoDigito s