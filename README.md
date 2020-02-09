# CI-3661 Proyecto I: Jack 'El Monadico' Lambda
Miembros: 
Diego Peña 15-
Wilfredo Graterol 15-10639

## Como correr el programa
Para ello es necesario correr el comando `make` desde el terminal, estando en el directorio donde se encuentra el proyecto. Despues de esto se corre el programa JackLambda.

## Detalles de implementacion
En el proyecto se desarrollaron todas las funciones que sin modificar sus firmas, sin embargo fueron necesarias algunas funciones auxiliares para implementarlas. En el modulo principal `jackLambda.hs` hay funciones que pueden parecer un poco confusas, como el caso de hit que recibe un parametro booleano el cual indica si la jugada es un hit normal o un doubleDown. Sin embargo, si se ee con suficiente entendimiento (mas alla de los nombres) el codigo es entendible. 

Para algunas funciones que reciben argumentos del tipo `Maybe`, al no especificarse que accion tomar al recibir un argumento con el valor `Nothing` se decidio una accion que permitiera continuar el juego.

En el caso de recibir las elecciones de los usuarios al elegir de que mazo sacar una carta, se decidio utilizar una representacion en Strings en vez de hacer una instancia read para ello por simplicidad de implementacion.