import System.IO
import System.Random
import System.Directory

data GameState = GS {
                    juegosJugados :: Int,
                    victoriasLambda :: Int,
                    nombre :: String,
                    generador :: StdGen,
                    dinero :: Int,
                    objetivo :: Int,
                    apuesta :: Int
                    } deriving (Show) --Hay que quitar este deriving después

main = do 
    x <- iniciarPartida
    --print x Esto era pare verificar si estaba guardando bien las cosas

extraerContenidos :: FilePath -> IO GameState
extraerContenidos archivo = do
    withFile archivo ReadMode (\handle -> do
        contents <- hGetContents handle
        (\[jj, vLamb, nomb, gen, din, obj, ap] -> 
            return $ GS (read jj) (read vLamb) nomb (read gen) (read din) (read obj) (read ap)) $ lines contents)
            --No hice map read porque me iban a quedar elementos de distintos tipos en la lista y eso no se puede


leerArchivo :: IO GameState
leerArchivo = do
    putStrLn "Ingrese el nombre del archivo: "
    nombre <- getLine
    existencia <- doesFileExist nombre
    if existencia then extraerContenidos nombre else do putStrLn $ "El archivo " ++ nombre ++ " no existe" 
                                                        iniciarPartida

solicitarDatos :: IO GameState
solicitarDatos = do
    putStrLn "Ingrese su nombre: "
    nombre <- getLine
    gen <- getStdGen
    dinero <- revisarCantidad 0 (maxBound :: Int) "Ingrese la cantidad de dinero inicial con que se quiere contar (Debe ser mayor a 0)"
    objetivo <- revisarCantidad dinero (maxBound :: Int) "Ingrese la cantidad de dinero que se debe alcanzar para ganar la partida (Debe ser mayor al monto seleccionado anteriormente)"
    apuesta <- revisarCantidad 0 dinero "Ingrese el monto a apostar en cada ronda (Debe ser menor que la cantidad de dinero inicial y  mayor que 0)"
    return $ GS 0 0 nombre gen dinero objetivo apuesta

revisarCantidad :: Int -> Int -> String -> IO Int
revisarCantidad montoMin montoMax mensaje = do
    putStrLn mensaje
    dineroStr <- getLine
    let dinero = (read dineroStr)::Int
    if montoMin < dinero && dinero <= montoMax then return dinero  else do putStrLn "Ese no es un monto válido"
                                                                           revisarCantidad montoMin montoMax mensaje

seleccionarAccion :: String -> IO GameState
seleccionarAccion "y" = do leerArchivo
seleccionarAccion "n" = do solicitarDatos
seleccionarAccion  x  = do putStrLn $ "La opción " ++ x ++ " no es válida" 
                           iniciarPartida

iniciarPartida :: IO GameState
iniciarPartida = do
    putStrLn "¿Desea cargar una partida? [y/n]"
    respuesta <- getLine
    seleccionarAccion respuesta