module Library where
import PdePreludat

data Auto = Auto {
    color :: Color,
    velocidad :: Number,
    distancia :: Number
}deriving (Show,Eq)

-- Carrera (Posicion,)

data Color = Rojo | Verde | Amarillo | Azul deriving(Show,Eq)


laDistanciaEsMenorA10 :: Auto -> Auto -> Bool
laDistanciaEsMenorA10 unAuto deOtroAuto = (distancia unAuto - distancia deOtroAuto) <10

losAutosSonDiferentes :: Auto -> Auto -> Bool
losAutosSonDiferentes unAuto deOtroAuto = color unAuto /= color deOtroAuto

estaCerca :: Auto -> Auto -> Bool
estaCercaunAuto unAuto deOtroAuto = laDistanciaEsMenorA10 unAuto deOtroAuto && losAutosSonDiferentes unAuto deOtroAuto
