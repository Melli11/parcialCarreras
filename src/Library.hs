module Library where
import PdePreludat

data Auto = Auto {
    color :: Color,
    velocidad :: Number,
    distancia :: Number
}deriving (Show,Eq)

type Carrera = ( Auto , Posicion)
type Posicion = Number

data Color = Rojo | Verde | Amarillo | Azul deriving(Show,Eq)

autoRojo = Auto Rojo 100 250
autoVerde = Auto Verde 75 200
autoAzul = Auto Azul 50 150

listaDeAutosWinRojo = [autoRojo,autoVerde,autoAzul]


laDistanciaEsMenorA10 :: Auto -> Auto -> Bool
laDistanciaEsMenorA10 unAuto deOtroAuto = abs(distancia unAuto - distancia deOtroAuto) <10

losAutosSonDiferentes :: Auto -> Auto -> Bool
losAutosSonDiferentes unAuto deOtroAuto = color unAuto /= color deOtroAuto

estaCerca :: Auto -> Auto -> Bool
estaCerca unAuto deOtroAuto = laDistanciaEsMenorA10 unAuto deOtroAuto && losAutosSonDiferentes unAuto deOtroAuto

-- vaTranquilo :: Auto -> Auto -> Bool 
-- vaTranquilo unAuto = not.estaCerca unAuto && lesVaGanandoATodos

lesVaGanandoATodos :: Auto -> [Auto] -> Bool
lesVaGanandoATodos unAuto = all (mayorDistanciaDeUnAuto unAuto) . filter (/= unAuto)

-- con filter (/= unAuto) me quedo con todos los autos diferentes del que estoy preguntando

mayorDistanciaDeUnAuto ::Auto -> Auto -> Bool
mayorDistanciaDeUnAuto unAuto otroAuto = distancia unAuto > distancia otroAuto 
