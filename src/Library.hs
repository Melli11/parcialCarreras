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
  -- auto = Auto Color Vel Dist

autoAmarillo = Auto Amarillo 50 300
autoRojo = Auto Rojo 100 250
autoVerde = Auto Verde 75 200
autoAzul = Auto Azul 50 150

listaDeAutosWinRojo = [autoRojo,autoVerde,autoAzul,autoAmarillo]

laDistanciaEsMenorA10 :: Auto -> Auto -> Bool
laDistanciaEsMenorA10 unAuto deOtroAuto = abs(distancia unAuto - distancia deOtroAuto) <10

losAutosSonDiferentes :: Auto -> Auto -> Bool
losAutosSonDiferentes unAuto deOtroAuto = color unAuto /= color deOtroAuto

estaCerca :: Auto -> Auto -> Bool
estaCerca unAuto deOtroAuto = laDistanciaEsMenorA10 unAuto deOtroAuto && losAutosSonDiferentes unAuto deOtroAuto

-- -- vaTranquilo :: Auto -> Auto -> Bool 
-- vaTranquilo :: Auto -> [Auto] -> Bool
nadieCerca :: Auto -> [Auto] -> Bool
nadieCerca unAuto =    any ((False ==) . estaCerca unAuto) . filter (/= unAuto)

vaTranquilo :: Auto -> [Auto] -> Bool
vaTranquilo unAuto competidores = nadieCerca unAuto competidores && lesVaGanandoATodos unAuto competidores

lesVaGanandoATodos :: Auto -> [Auto] -> Bool
lesVaGanandoATodos unAuto = all (mayorDistanciaDeUnAuto unAuto) . filter (/= unAuto)

-- con filter (/= unAuto) me quedo con todos los autos diferentes del que estoy preguntando

mayorDistanciaDeUnAuto ::Auto -> Auto -> Bool
mayorDistanciaDeUnAuto unAuto otroAuto = distancia unAuto > distancia otroAuto

-- autosQueLoSuperan :: Auto ->
autosQueLoSuperan :: Auto -> [Auto] -> Number
autosQueLoSuperan unAuto = length.filter (==False) . map (mayorDistanciaDeUnAuto unAuto) . filter (/= unAuto)

enQuePuestoEsta :: Auto -> [Auto] -> Number
enQuePuestoEsta unAuto listaDeAutos= 1 + autosQueLoSuperan unAuto listaDeAutos

-- 2. Desarrollar las funciones necesarias para manipular el estado de los autos para que sea posible:

-- Hacer que un auto corra durante un determinado tiempo. Luego de correr la cantidad de tiempo
-- indicada, la distancia recorrida por el auto debería ser equivalente a la distancia que llevaba
-- recorrida + ese tiempo * la velocidad a la que estaba yendo

correrDuranteUnTiempo :: Auto -> Number -> Auto
correrDuranteUnTiempo auto tiempo = auto {distancia = distancia auto + tiempo * velocidad auto}


alterarLaVelocidad' :: (Number -> Number ) -> Auto -> Auto
alterarLaVelocidad'  modificador auto = auto {velocidad = modificador (velocidad auto)}   

alterarLaVelocidad:: (Number -> Number ) -> Auto  -> Auto
alterarLaVelocidad    modificador auto = auto {velocidad = modificador.velocidad $auto}   

bajarVelocidad :: Number -> Auto -> Auto
bajarVelocidad cantidad = alterarLaVelocidad ( max 0 . subtract cantidad )

-- :t subtract 
-- subtract :: Number -> Number -> Number
-- Ejemplo de uso: Substract resta el primer numerico que recibe al segundo

-- Input: subtract 3 5
-- Output: 2


-- bajarVelocidad :: Number -> Auto -> Auto
-- bajarVelocidad :: Number -> Auto -> Auto
-- bajarVelocidad cantidad auto = auto { velocidad = min 0 (alterarLaVelocidad (- cantidad) (velocidad auto)  )}
