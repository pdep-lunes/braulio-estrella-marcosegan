module Lib () where

import Text.Show.Functions ()

doble :: Int -> Int
doble x = x * 2

data Personaje = ConstruirPersonaje {
        nombre :: String,
        poderBasico :: Poder,
        superPoder :: Poder,
        superPoderEstaActivo :: Bool,
        vida :: Int
    }
    deriving Show

type Poder = Personaje -> Personaje
type TipoDeTuerca = String
type Contrincante = Personaje

-- Poderes

quitarVida :: Contrincante -> Int -> Contrincante
quitarVida personaje danio
    | vida personaje >= danio = personaje { vida = vida personaje - danio }
    | otherwise = personaje { vida = 0 }

bolaEspinosa :: Poder
bolaEspinosa contrincante = quitarVida contrincante 1000

lluviaDeTuercas :: TipoDeTuerca -> Contrincante -> Contrincante
lluviaDeTuercas tipoDeTuerca contrincante
    | tipoDeTuerca == "Sanadora" = contrincante { vida = vida contrincante + 800 }
    | tipoDeTuerca == "Dañina" = contrincante { vida = vida contrincante `div` 2 }
    | otherwise = contrincante

-- Cuando el radio de explosión es mayor a 3, es una potente :O
granadaDeEspinasPotente :: Poder
granadaDeEspinasPotente contrincante
    | vida contrincante < 800 = contrincante {
            superPoderEstaActivo = False,
            vida = 0,
            -- TODO: ¿Por qué estoy repitiendo lo mismo? Debería ser más fácil
            nombre = nombre contrincante ++ "Espina estuvo aquí"
        }
    | otherwise = contrincante { nombre = nombre contrincante ++ "Espina estuvo aquí" }

granadaDeEspinas :: Int -> Contrincante -> Contrincante
granadaDeEspinas radioDeExplosion contrincante
    | radioDeExplosion > 3 = granadaDeEspinasPotente contrincante
    | otherwise = contrincante

torretaCurativa :: Poder
torretaCurativa aliado = aliado { superPoderEstaActivo = True, vida = doble $ vida aliado }

-- Reportes

atacarConPoderEspecial :: Personaje -> Contrincante -> Contrincante
atacarConPoderEspecial personaje contrincante
    | superPoderEstaActivo personaje = poderBasico personaje.superPoder personaje $ contrincante
    | otherwise = contrincante

brawlersQueEstanEnLasUltimas :: [Personaje] -> [Personaje]
brawlersQueEstanEnLasUltimas personajes = filter ((<800).vida) personajes
