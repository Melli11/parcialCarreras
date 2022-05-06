module Spec where
import PdePreludat
import Library
import Test.Hspec


correrTests :: IO ()
correrTests = hspec $ do
  suiteDeTestsDeParte1
-- suiteDeTestsDeParte2
-- suiteDeTestsDeParte3
-- suiteDeTestsDeParte4
suiteDeTestsDeParte1 = describe "Concediendo Deseos" $ do
  -- auto = Auto Color Vel Dist
  let autoRojo = Auto Rojo 100 250
  let autoVerde = Auto Verde 75 200
  let autoAzul = Auto Azul 50 195
  
  describe "Auto y Carrera" $ do
    it "Un auto  está cerca de otro si son autos distintos y la distancia que hay entre ellos (en valor absoluto) es menor a 10 " $ do
       estaCerca autoVerde autoAzul `shouldBe`  True
    it "Un auto NO está cerca de otro si son autos iguales" $ do
       estaCerca autoRojo autoRojo `shouldBe`  False
    it "Un auto  NO está cerca de otro si la distancia que hay entre ellos (en valor absoluto) es menor a 10 " $ do
       estaCerca autoVerde autoRojo `shouldBe`  False
  
    -- it "Un auto va tranquilo si no tienen ningun auto cerca y les va gananado a todos" $ do
    --    `shouldBe` 0 
  --   it "En que puesto está" $ do
  --     length (habilidad (aprenderHabilidades [] chicoConHabilidad)) `shouldBe` 1 
  -- describe "Mayoria de Edad" $ do
  --   it "Sea un niño y le cumplo el deseo de ser Mayor entonces tendrá 18" $ do
  --     edad (serMayor chico) `shouldBe` 18 

-- suiteDeTestsDeParte2 = describe "Padrinos magicos" $ do
--   let chicoSinHabilidadCon2Deseos = Chico "bobi" 10 [] [aprenderHabilidades ["Andar en bicicleta"],serMayor]
--   let chicoConPrimerDeseoCumplido = Chico "bobi" 10 ["Andar en bicicleta"] [serMayor]
--   describe "Wanda" $ do
--     it "Dado un chico, wanda le cumple el primer deseo " $ do
--       length (habilidad (wanda chicoSinHabilidadCon2Deseos)) `shouldBe`  1
--     it "Dado un chico, wanda lo hace madurar  " $ do
--       edad (wanda chicoSinHabilidadCon2Deseos) `shouldBe`  11
--   describe "Cosmo" $ do
--     it "Dado un chico, cosmo lo hace desmadurar, quedando con la mitad de años de edad" $ do
--       edad (cosmo chicoSinHabilidadCon2Deseos) `shouldBe`  5
--     it "Como cosmo es olvidadizo, no le concede ningún deseo." $ do
--       length (habilidad (cosmo chicoSinHabilidadCon2Deseos)) `shouldBe`  0
--   describe "muffinMagico" $ do
--     it "Dado un chico le concede todos sus deseos, ser mayor y aprender a bicicletear." $ do
--       length (habilidad(muffinMagico chicoSinHabilidadCon2Deseos)) `shouldBe` 1 
--       edad (muffinMagico chicoSinHabilidadCon2Deseos) `shouldBe`  18 