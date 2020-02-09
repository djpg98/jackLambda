-- Wilfredo Graterol
-- Diego Peña

module Cartas 
( Carta
, Mano (..)
, Mazo (..)
, Eleccion (..)
, Jugador (..)
, baraja
, valor
, cantidadCartas
, barajar
, busted
, inicialLambda
, blackjack
, desdeMano
, reconstruir
, robar
, juegaLambda
, ganador
)where

import Prelude
import Data.List
import System.Random

-- Tipo de dato "Palo" e implemntación de instancias

data Palo = Treboles | Diamantes | Picas | Corazones

instance Eq Palo where 
    (==) Treboles Treboles = True
    (==) Diamantes Diamantes = True
    (==) Picas Picas = True
    (==) Corazones Corazones = True
    (==) _ _ = False
    
    (/=) a b = not (a==b)

instance Show Palo where
    show Treboles = "♣"
    show Diamantes = "♦"
    show Picas = "♠"
    show Corazones = "♥"

-- Tipo de dato "Rango" e implemntación de instancias

data Rango = N Int | Jack | King | Queen | Ace

instance Eq Rango where
    (==) Jack Jack = True
    (==) Queen Queen = True
    (==) King King = True
    (==) Ace Ace = True
    (==) (N a) (N b) = a==b
    (==) _ _ = False
    
    (/=) a b = not (a==b)

instance Show Rango where
    show (N a) = show a
    show Jack = "J"
    show King = "K"
    show Queen = "Q"
    show Ace = "A" 

-- Tipo de dato "Carta" e implemntación de instancias

data Carta = Carta {
                    rango :: Rango,
                    palo :: Palo
                   }

instance Eq Carta where
    (==) (Carta {rango = rga, palo = pa}) Carta {rango = rgb, palo = pb} = if rga==rgb && pa==pb then True else False

    (/=) a b = not (a==b)

instance Show Carta where
    show (Carta {rango = rg, palo = p}) = show p ++ "" ++ show rg

-- Tipo de dato "Jugador" e implemntación de instancias

data Jugador = Dealer | Player

instance Eq Jugador where
    (==) Dealer Dealer = True
    (==) Player Player = True
    (==) _       _      = False
    (/=) a      b       = not (a == b)

instance Show Jugador where
     show Dealer = "Dealer"
     show Player = "Player"

instance Read Jugador where
    readsPrec _ "Dealer" = [(Dealer, " ")]
    readsPrec _ "Player" = [(Player, " ")]

-- Tipo de dato "Mano" e implemntación de instancias

newtype Mano = Mano [Carta]

instance Show Mano where
    show (Mano c) = concatMap (show) c

-- Tipo de dato "Mazo"

data Mazo = Vacio | Mitad Carta Mazo Mazo

-- Tipo de dato 

data Eleccion = Izquierdo | Derecho


{--
Funciones relacionadas con el tipo de dato "Mano"
--}

-- Crea una mano vacía
vacia :: Mano
vacia = Mano []

-- Crea listas correspondientes a cada palo y luego las une en una sola para formar la baraja completa
baraja :: Mano
baraja =    
    let rangos = map (N) [2..10] ++ [Jack, King, Queen, Ace]
        crearPalo p = map (\rg -> Carta {rango = rg, palo = p}) rangos
    in Mano $ concatMap (crearPalo) [Treboles, Diamantes, Picas, Corazones]

-- Calcula la cantidad de cartas en una mano
cantidadCartas :: Mano -> Int
cantidadCartas (Mano c) = length c

-- Calcula el valor de una carta particular
valorCarta :: Carta -> Int
valorCarta (Carta (N i) _) = i
valorCarta (Carta Ace _)   = 11
valorCarta (Carta _ _)     = 10

-- Calcula el valor total de las cartas en una mano
valor :: Mano -> Int
valor (Mano c) =
    let (sumaDeNoAces, cantidadAces) = (\(x, y) -> (sum x, length y)) $ span (/= 11) $ sort (map (valorCarta) c)
        (suma11, suma1) = (sumaDeNoAces + 11 * cantidadAces, sumaDeNoAces + cantidadAces)
    in  if suma11 <= 21 then suma11 else suma1

-- Determina si el valor de las cartas de una mano supera 21
busted :: Mano -> Bool
busted mano = if (valor mano) > 21 then True else False


-- Determina si una mano es blackjack
blackjack :: Mano -> Bool
blackjack mano = if valor mano == 21 then True else False

-- Se determina si gano el player o el dealer
ganador :: Mano -> Mano -> Jugador
ganador manoDealer manoPlayer = if (valor manoDealer) >= (valor manoPlayer) then Dealer else Player

-- Separa la mano en dos partes iguales o casi iguales y una carta central entre las mismas. Es útil para el mazo
separar :: Mano -> (Mano, Carta, Mano)
separar (Mano []) = error "No hay cartas en la mano"
separar (Mano c) =
    let mitad      = length c `div` 2
        (izq, der) = (take mitad c, drop mitad c)
    in (Mano izq, head der, Mano (tail der))

-- Crea una mano barajada
barajar :: StdGen -> Mano -> Mano
barajar gen (Mano c) = Mano (barajarAux gen c)

-- Dada una lista de cartas, devuelve una lista con las mismas cartas pero ordenadas aleatoriamente
-- No estoy claro si la frase "ordenadas aleatoriamente" tiene sentido, pero se entiende la idea 
barajarAux :: StdGen -> [Carta] -> [Carta]
barajarAux _ [] = []
barajarAux gen listaCartas =
    let (index, newGen) = randomR(0, subtract 1 $ length listaCartas) gen :: (Int, StdGen)
        carta = listaCartas !! index
    in carta:(barajarAux newGen $ delete carta listaCartas)

-- Selecciona las primeras dos cartas para la mano de Jack Lambda y las retira de la mano original de todas las cartas
inicialLambda :: Mano -> (Mano, Mano)
inicialLambda (Mano c) = (Mano $ take 2 c, Mano $ drop 2 c)

{--
Funciones relacionadas con el tipo de dato "Mazo"
--}

-- Dada una mano, devuelve un elemento de tipo mazo conformado por las mismas cartas
desdeMano :: Mano -> Mazo
desdeMano (Mano []) = Vacio
desdeMano mano = desdeManoAux $ separar mano

-- Dada una mano separada utilizando la función separar, se crea un mazo con las mismas cartas
desdeManoAux :: (Mano, Carta, Mano) -> Mazo
desdeManoAux (Mano [], center, Mano []) = Mitad center Vacio Vacio
desdeManoAux (Mano [], center, right) = Mitad center Vacio (desdeMano right)
desdeManoAux (left, center, Mano []) = Mitad center (desdeMano left) Vacio
desdeManoAux (left, center, right) =  Mitad center (desdeMano left) (desdeMano right)

-- Determina si es posible seguir picando el mazo
puedePicar :: Mazo -> Bool
puedePicar (Vacio) = False
puedePicar (Mitad _ Vacio Vacio) = False
puedePicar (Mitad _ _ _) = True 

-- Convierte un mazo en una mano, formada por las mimas cartas
aplanar :: Mazo -> Mano
aplanar Vacio = vacia
aplanar (Mitad carta left right) = let
    Mano listaIzq = aplanar left
    listaCent = [carta]
    Mano listaDer = aplanar right
    in Mano (listaIzq++listaCent++listaDer)

{-- Recibe el Mazo original y una Mano con las cartas jugadas en la ronda. Debe eliminar todas las cartas jugadas del Mazo,
    y luego debe reconstruirlo --}
reconstruir :: Mazo ->Mano ->Mazo
reconstruir mazo mano = desdeMano $ (\ (Mano listaMazo) (Mano listaMano) -> Mano (listaMazo \\ listaMano)) (aplanar mazo) mano

{-- Recibe el Mazo actual, la mano del jugador y una Eleccion. Debe devolver una tupla con el Maz resultante (la Eleccion 
    indica qué hijo del Mazo se usará) y la Mano resultante.
    Nota: Aqui se depuran las cartas repetidas--}
robar :: Mazo -> Mano -> Eleccion -> Maybe (Mazo,Mano)
robar Vacio _ _ = Nothing
robar (Mitad _ Vacio Vacio) _ _ = Nothing
robar (Mitad _ left Vacio) _ Derecho = Nothing
robar (Mitad _ Vacio right) _ Izquierdo = Nothing
robar mazo@(Mitad _ (Mitad leftCenter _ _) _) (Mano listaMano) Izquierdo = 
    Just (reconstruir mazo (Mano (leftCenter:listaMano)), (Mano (leftCenter:listaMano)))
robar mazo@(Mitad _ _ (Mitad rightCenter _ _)) (Mano listaMano) Derecho = 
    Just (reconstruir mazo (Mano (rightCenter:listaMano)), (Mano (rightCenter:listaMano)))

{-- Recibe el Mazo actual, lo vuelve a convertir en una Mano en el orden apropiado, recibe la Mano del dealer,
   y devuelve la Mano resultante de robar hasta que supere un valor de 16 --}
juegaLambda :: Mazo ->Mano ->Maybe Mano
juegaLambda Vacio _ = Nothing
juegaLambda mazo mano = let
    Mano (x:xs) = aplanar mazo
    Mano listaMano = mano
    in if valor mano > 16 then Just mano else juegaLambda (desdeMano (Mano xs)) (Mano (x:listaMano))

manoPrueba = Mano [Carta (N 10) Corazones, Carta (N 4) Picas, Carta (N 2) Treboles, Carta Queen Picas]