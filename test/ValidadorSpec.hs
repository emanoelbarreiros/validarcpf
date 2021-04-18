module ValidadorSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Text (Text,unpack)
import Control.Monad
import Text.Printf
import Validador

spec :: Spec
spec = do
    describe "digitos" $ do
        context "quando passada uma string" $ do
            it "retorna a representacao [int] da string" $ do
                digitos "123456789" == [1,2,3,4,5,6,7,8,9]

    describe "validarPrimeiroDigito" $ do
        context "quando passado um CPF com primeiro digito valido" $ do
            it "valida o CPF" $ do
                validarPrimeiroDigito "51171591870" `shouldBe` True
        context "quando passado um CPF com primeiro digito invalido" $ do
            it "invalida o CPF" $ do
                validarPrimeiroDigito "51171591800" `shouldBe` False

    describe "validarSegundoDigito" $ do
        context "quando passado um CPF com segundo digito valido" $ do
            it "valida o CPF" $ do
                validarSegundoDigito "51171591870" `shouldBe` True
        context "quando passado um CPF com segundo digito invalido" $ do
            it "invalida o CPF" $ do
                validarSegundoDigito "51171591879" `shouldBe` False

    describe "valido" $ do
        context "quando passado um CPF valido" $ do
            it "valida o CPF" $ do
                valido "51171591870" `shouldBe` True
        context "quando passado um CPF com primeiro digito invalido e segundo valido" $ do
            it "invalida o CPF" $ do
                valido "51171591890" `shouldBe` False
        context "quando passado um CPF com primeiro digito valido e segundo invalido" $ do
            it "invalida o CPF" $ do
                valido "51171591879" `shouldBe` False
        context "quando passado um CPF com ambos os digitos invalidos" $ do
            it "invalida o CPF" $ do
                valido "51171591899" `shouldBe` False
        

    describe "validarDigito" $ do
        context "quando solicita validacao do 10o digito" $ do
            it "valida o digito" $ do
                validarDigito "05080508477" 10 `shouldBe` True 
        context "quando solicita um digito invalido" $ do
            prop "invalida o CPF" $ do
                \x -> x /= 10 && x /= 11 ==> validarDigito "05080508477" x `shouldBe` False

    exemplos <- runIO lerExemplos
    forM_ exemplos $ \cpf ->
        describe "valido" $ do
            context "quando alimentado com exemplos válidos" $ do
                it (printf "exemplo %s" cpf) $ do
                    valido (unpack cpf) `shouldBe` True

lerExemplos :: IO [Text]
lerExemplos = Text.lines <$> Text.readFile "test/exemplos.txt"