-- Para evitar complicarnos al definir funciones para obtener el autor de un libro o la cantidad 
-- de págs. se puede modelar como: 
-- type Libro = (String, String, Int)
-- Para lo que necesitamos modelar nos va a ser suficiente, y de hecho nos va a facilitar
-- al momento de definir las funciones, y no tener que usar algo como:
-- obtenerCantidadPaginas unLibro =  fst (snd unLibro)

type Libro = (String,(String,Int))
elVisitante :: Libro
elVisitante = ("El Visitante",("Stephen King",592))
oyasumiPunpun1 :: Libro
oyasumiPunpun1 = ("Oyasumi Punpun 1",("Asano Inio",40))
oyasumiPunpun3 :: Libro
oyasumiPunpun3 = ("Oyasumi Punpun 3",("Asano Inio",40))
oyasumiPunpun127 :: Libro
oyasumiPunpun127 = ("Oyasumi Punpun 127",("Asano Inio",40))
fundacion :: Libro
fundacion = ("Fundacion",("Isaac Asimov" , 230))
sandman5 :: Libro
sandman5 = ("Sandman 5",("Neil Gaiman",35))
sandman10 :: Libro
sandman10 = ("Sandman 10",("Neil Gaiman",35))
sandman12 :: Libro
sandman12 = ("Sandman 12",("Neil Gaiman",35))
eragon :: Libro
eragon = ("Eragon",("Christopher Paolini",544))
eldest :: Libro
eldest = ("Eldest",("Christopher Paolini",704))
brisignr :: Libro
brisignr = ("Brisgnr",("Christopher Paolini",700))
legado :: Libro
legado = ("Legado",("Christopher Paolini",811))
type Biblioteca = [Libro]
type Saga = [Libro]
sagaEragon :: Saga
sagaEragon = [eragon,eldest,brisignr,legado]
miBiblioteca :: Biblioteca
miBiblioteca = [elVisitante,oyasumiPunpun1,oyasumiPunpun3,oyasumiPunpun127,fundacion,sandman5,sandman10,sandman12,eragon,eldest,brisignr,legado]

-- En la guía de lenguajes ya tenemos una función para invertir el orden de los parámetros
-- que recibe una función: flip! 
divInversa x y = div y x

-- Estaría buenísimo delegar en pequeñas funciones y abstraer parte de la lógica de promedioDeHojas.
-- De esa forma podríamos ser más declarativos.
-- Por ejemplo, definiendo una función que obtenga el total de hojas de una biblioteca y otra la cantidad
-- de libros, para después calcular sólo el promedio en la función "principal".
-- De esa forma, la función hace únicamente lo que dice que hace (saquemos provecho de la expresividad).
-- Recordar que numero de hojas no es lo mismo que numeo de págs. Habría que hacer nroDePags/2.

promedioDeHojas :: Biblioteca -> Int
promedioDeHojas biblioteca= ((divInversa (length biblioteca)).sum.(map (snd.snd))) biblioteca

-- Para mejorar la expresividad, se puede definir la función como "esLecturaObligatoria".
-- De hecho así también resulta más fácil darse cuenta que el retorno de la función es en realidad un Bool,
-- ya que estás esperando que devuelva un resultado de dos disyunciones. 

-- Las tuplas pueden compararse de acuerdo con todo su contenido.
-- Se podría abstraer una función de (fst (snd libro)) == "Stephen King", poniéndole un nombre expresivo.
-- Ya que como está escrito, no sabemos realmente que es lo que está haciendo, más que manejar una tupla.
-- El enunciado pide saber si una lectura es obligatoria, no pide lo mismo para la biblioteca. Entonces lo que 
-- definiste como lectura obligatoria para listas estaría de más.
-- Además, de que estás definiendo un filter con guardas, si quisieras obtener los libros que son 
-- de lectura obligatoria de una biblioteca, podrías hacerlo usando un filter con la condición que necesitás.

lecturaObligatoria :: Biblioteca -> [Libro]
esObligatoria libro = elem libro sagaEragon || fst libro == "Fundacion" || (fst (snd libro)) == "Stephen King"
lecturaObligatoria [] = []
lecturaObligatoria (x:xs) | esObligatoria x = x:(lecturaObligatoria xs)
 |otherwise = lecturaObligatoria xs

-- Fijate que estás repitiendo la lógica para chequear si un libro es de determinado autor, tanto acá como
-- cuando lo verificaste para el autor "Stephen King" más arriba. Lo más conveniente sería definir una función
-- que dado un autor y un libro, pueda decirte si el libro es de ese autor.
-- De esa forma, vás a poder reutilizar todas las veces que quieras la función, pasándole por parámetro a distintos
-- autores, y aplicándola parcialmente. Ni hablar de que vas a salvar varios gatitos con la abtracción de la lógica.
-- Podría ser algo como:
-- esLibroDe unAutor unLibro = ((==unAutor).fst.snd) unLibro 

-- Para mejorar la expresividad, se puede definir la función como "esFantasiosa".
-- Recordá que en lugar de utilizar las funciones map y elem conjuntamente, podemos verificar 
-- si algún elemento de la lista cumple con una condición con una única función: any.
-- Así hacemos uso de la declaratividad.

fantasiosa :: Biblioteca -> Bool
fantasiosa biblioteca = (elem "Christopher Paolini" (map (fst.snd) biblioteca)) || (elem "Neil Gaiman" (map (fst.snd) biblioteca))

-- Recordá considerar también las mayúsculas :)
esVocal x = elem x "aeiou"

-- Nuevamente, en lugar de guardas podemos utilizar una función que ya tenemos y conocemos: filter!

quitarVocales [] = []
quitarVocales (x:xs) | esVocal x = quitarVocales xs
 |otherwise = x : (quitarVocales  xs)

-- Existe una función muy bella que está en la guía de lenguajes, y nos permite hacer
-- las dos cosas en una única aplicación: concatMap!

nombreDeLaBiblioteca :: Biblioteca -> String
nombreDeLaBiblioteca = quitarVocales.concat.(map (concat.words.fst))

-- Recomendable (muuy muuuy recomendable) delegar la función para conocer la cantidad de hojas o paginas de un libro
-- y luego utilizarla para ser lo más declarativos posibles. Si ya estás usando un all, el map no debería ser necesario.
-- Podés simplemente hacer una función que chequee exactamente lo que vos necesitás y definirlo como:
-- bibliotecaLigera unaBiblioteca = all (condicion) unaBiblioteca
-- Con lo cual nos evitamos el choclazo.

bibliotecaLigera :: Biblioteca -> Bool
bibliotecaLigera biblioteca = all (<41) (map (snd.snd) biblioteca)

-- Por favor poner espacios entre las funciones y valores definidos :)