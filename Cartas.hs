-- Wilfredo Graterol
-- Diego Peña

import Data.List

data Palo = Treboles | Diamantes | Picas | Corazones

instance Show Palo where
    show Treboles = "♣"
    show Diamantes = "♦"
    show Picas = "♠"
    show Corazones = "♥"


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
    show (Carta {rango = rg, palo = p}) = show p ++ " " ++ show rg

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

-- Antes podía haber casos donde tomara unos aces como 11 y otros como 1, pero ya lo arreglé
-- No quedó tan bonito sin embargo
valor :: Mano -> Int
valor (Mano c) =
    let (sumaDeNoAces, cantidadAces) = (\(x, y) -> (sum x, length y)) $ span (/= 11) $ sort (map (valorCarta) c)
        (suma11, suma1) = (sumaDeNoAces + 11 * cantidadAces, sumaDeNoAces + cantidadAces)
    in  if suma11 <= 21 then suma11 else suma1

busted :: Mano -> Bool
busted mano = if valor mano > 21 then True else False


--En principio no le voy a poner que verifique que la mano tenga dos cartas, porque la idea sería solo llamarla
-- cuando la mano tenga dos cartas
blackjack :: Mano -> Bool
blackjack mano = if valor mano == 21 then True else False

ganador :: Mano -> Mano -> Jugador
ganador manoDealer manoPlayer = if valor manoDealer >= valor manoPlayer then Dealer else Player

separar :: Mano -> (Mano, Carta, Mano)
separar (Mano c) =
    let mitad      = length c `div` 2
        (izq, der) = (take mitad c, drop mitad c)
    in (Mano izq, head der, Mano (tail der))

-- Esto es para probar las funciones sobre manos que no sean la baraja completa o vacía
manoPrueba = Mano [
                    Carta Jack Treboles,
                    Carta (N 1) Diamantes,
                    Carta Ace Picas
                  ]

manoPrueba2 = Mano [
                    Carta Jack Treboles,
                    Carta (N 1) Diamantes,
                    Carta (N 9) Diamantes,
                    Carta Ace Picas
                  ]

