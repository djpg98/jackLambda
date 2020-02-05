import System.IO
import System.Random
import System.Directory

--Note mental: Implementar Read para GameState

data GameState = GS {
                    juegosJugados :: Int,
                    victoriasLambda :: Int,
                    nombre :: String,
                    generador :: StdGen,
                    dinero :: Int,
                    objetivo :: Int,
                    apuesta :: Int
                    } deriving (Show) --Hay que quitar este deriving después

{-- instance Read GameState where
    read GS {juegosJugados = jj, victoriasLambda = vL, nombre = nomb, generador  = gen, dinero = din, 
    objetivo = obj, apuesta = ap} = 
        if verificarCantidades --}

main = do 
    x <- iniciarPartida
    menuPrincipal x

{-- Esta es la función que lee del archivo y guarda los datos en un gamestate. En el futuro estará implmentada con un
    read para gamestate --}
extraerContenidos :: FilePath -> IO GameState
extraerContenidos archivo = do
    withFile archivo ReadMode (\handle -> do
        contents <- hGetContents handle
        (\[jj, vL, nomb, gen, din, obj, ap] -> 
            return $ GS (read jj) (read vL) nomb (read gen) (read din) (read obj) (read ap)) $ lines contents)
            --No hice map read porque me iban a quedar elementos de distintos tipos en la lista y eso no se puede


{-- Esta función verifica que exista el archivo que se desea cargar. Si existe, llama a extraerContenidos, si no se
    devuelve al menú desde donde se fue llamado (Recordar que se puede cargar desde dos puntos distintos del juego)
    menuDeRetorno le indica el menú al que puede volver: menuInicial o menuPrincipal--}
leerArchivo :: IO GameState -> IO GameState
leerArchivo menuDeRetorno = do
    putStrLn "Ingrese el nombre del archivo: "
    nombre <- getLine
    existencia <- doesFileExist nombre
    if existencia then extraerContenidos nombre else do putStrLn $ "El archivo " ++ nombre ++ " no existe" 
                                                        menuDeRetorno

{-- Solicita los datos del jugador, llama a las funciones de verificación y crea un gameState nuevo --}
solicitarDatos :: IO GameState
solicitarDatos = do
    putStrLn "Ingrese su nombre: "
    nombre <- getLine
    gen <- getStdGen
    dinero <- revisarCantidad 0 (maxBound :: Int) "Ingrese la cantidad de dinero inicial con que se quiere contar (Debe ser mayor a 0)"
    objetivo <- revisarCantidad dinero (maxBound :: Int) "Ingrese la cantidad de dinero que se debe alcanzar para ganar la partida (Debe ser mayor al monto seleccionado anteriormente)"
    apuesta <- revisarCantidad 0 dinero "Ingrese el monto a apostar en cada ronda (Debe ser menor que la cantidad de dinero inicial y  mayor que 0)"
    return $ GS 0 0 nombre gen dinero objetivo apuesta

{-- Esta función verifica que la cantidad de dinero sea válida de acuerdo a las normas establecidas por el juego
    Si se introduce una cantidad que no se corresponde con las mismas, se le da al usuario la oportunidad de
    introducir de nuevo el valor--}
revisarCantidad :: Int -> Int -> String -> IO Int
revisarCantidad montoMin montoMax mensaje = do
    putStrLn mensaje
    dineroStr <- getLine
    let dinero = (read dineroStr)::Int
    if montoMin < dinero && dinero <= montoMax then return dinero  else do putStrLn "Ese no es un monto válido"
                                                                           revisarCantidad montoMin montoMax mensaje

{-- Es la responsable de llamar a la acción del menú inicial (Cargar Juego, nuevo juego) que el usuario desea realizar --}
seleccionarAccion :: String -> IO GameState
seleccionarAccion "y" = do leerArchivo menuInicial
seleccionarAccion "n" = do solicitarDatos
seleccionarAccion  x  = do putStrLn $ "La opción " ++ x ++ " no es válida" 
                           menuInicial

{-- Despliega el menú inicial del juego --}
menuInicial :: IO GameState
menuInicial = do
    putStrLn "¿Desea cargar una partida? [y/n]"
    respuesta <- getLine
    seleccionarAccion respuesta

{-- Calcula la cantidad de partidasganadas por el usuario--}
victoriasUsuario :: GameState -> String
victoriasUsuario game = show $ juegosJugados game - victoriasLambda game

{-- Muestra los datos relevantes de la partida --}
mostarDatos :: GameState -> IO ()
mostarDatos game = do
    putStrLn $ "Partidas Jugadas: " ++ (show . juegosJugados $ game)
    putStrLn $ "Partidas ganadas por Jack Lambda: " ++ (show . victoriasLambda $ game)
    putStrLn $ "Partidas ganadas por " ++ nombre game ++ ": " ++ victoriasUsuario game
    putStrLn $ "Cantidad de dinero restante: " ++ (show . dinero $ game)

{-- Ejecuta la acción del menú principal seleccionada por el usuario --}
seleccionarOpcion :: String -> GameState -> IO GameState
seleccionarOpcion "3" game = do leerArchivo (menuPrincipal game)
                                menuPrincipal game
seleccionarOpcion "4" game = do return game
seleccionarOpcion  _  game = do putStrLn $ "Esa opción no es válida " ++ (nombre game)
                                menuPrincipal game

{-- Despliega el menú principal --}
menuPrincipal :: GameState -> IO GameState
menuPrincipal game = do
    mostarDatos game
    putStrLn $ "1 - Jugar ronda"
    putStrLn $ "2 - Guardar Partida"
    putStrLn $ "3 - Cargar Partida"
    putStrLn $ "4 - Salir"
    putStrLn $ "Ingrese el número de la opción a seleccionar por favor"
    opcion <- getLine
    seleccionarOpcion opcion game