-- Wilfredo Graterol
-- Diego Peña

import Data.List

data Palo = Treboles | Diamantes | Picas | Corazones deriving(Show) --Este deriving es temporal para hacer pruebas

{--
instance Show Palo where
    toDo
--}

data Rango = N Int | Jack | King | Queen | Ace

instance Show Rango where
    show (N a) = show a
    show Jack = "J"
    show King = "K"
    show Queen = "Q"
    show Ace = "A" 

data Carta = Carta {
                    rango :: Rango,
                    palo :: Palo
                   }

instance Show Carta where
    show (Carta {rango = rg, palo = p}) = show p ++ show rg

data Jugador = Dealer | Player deriving(Show, Read)

newtype Mano = Mano [Carta] --deriving(Show)

instance Show Mano where
    show (Mano c) = concatMap (show) c

data Mazo = Vacio | Mitad Carta Mazo Mazo

data Eleccion = Izquierdo | Derecho

{--
Funciones de mano
--}

vacia :: Mano
vacia = Mano []

{-- 
baraja :: Mano
baraja = Mano $ concatMap (crearPalo) [Treboles, Diamantes, Picas, Corazones]
    where crearPalo p = map (\rg -> Carta {rango = rg, palo = p}) $ map (N) [1..9] ++ [Jack, King, Queen, Ace]
--}

-- Crea listas correspondientes a cada palo y luego las une en una sola para formar la baraja
baraja :: Mano
baraja =    
    let rangos = map (N) [1..9] ++ [Jack, King, Queen, Ace]
        crearPalo p = map (\rg -> Carta {rango = rg, palo = p}) rangos
    in Mano $ concatMap (crearPalo) [Treboles, Diamantes, Picas, Corazones]

cantidadCartas :: Mano -> Int
cantidadCartas (Mano c) = length c

valorCarta :: Carta -> Int
valorCarta (Carta (N i) _) = i
valorCarta (Carta Ace _)   = 11
valorCarta (Carta _ _)     = 10

-- El primer argumento es el valor acumulado de las cartas de la mano que hemos sumado hasta ahora y el segundo 
-- es el valor de la carta a sumar
sumarCartas :: Int -> Int -> Int
sumarCartas acc 11
    | acc + 11 > 21 = acc + 1
    | otherwise     = acc + 11
sumarCartas acc actual = acc + actual

-- No estoy seguro de si es la forma más óptima, mañana lo pienso bien
valor :: Mano -> Int
valor (Mano c) =
    let valores = sort (map (valorCarta) c)
    in foldl' (sumarCartas) 0 valores

busted :: Mano -> Bool
busted mano = if valor mano > 21 then True else False


--En principio no le voy a poner que verifique que la mano tenga dos cartas, porque la idea sería solo llamarla
-- cuando la mano tenga dos cartas
blackjack :: Mano -> Bool
blackjack mano = if valor mano == 21 then True else False

-- Esto es para probar las funciones sobre manos que no sean la baraja completa o vacía
manoPrueba = Mano [
                    Carta Jack Treboles,
                    Carta (N 1) Diamantes,
                    Carta Ace Picas
                  ]

