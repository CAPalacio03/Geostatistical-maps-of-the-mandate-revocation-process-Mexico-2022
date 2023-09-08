                                ###### VISUALIZACION DE DATOS #######

# Para comenzar el análisis exploratorio de datos, primero debemos obtener la tabla de infmormación
# Utilizamos la libreria readr para ello
library(readr)
library(dplyr)
# Con la función "read_csv" exportamos el archivo "Revocacion de mandato 2022.csv" a R en un date frame
datos <- read.csv("Datos/20220411_1845_COMPUTOS_RM2022.csv", skip = 5)
datos <- select(datos, c(1,3:7,13:17))

# Para manejar los tablas en tibbles
library(tibble)

as_tibble(datos)

# Para poder visualizar los datos utilizaremos la libreria "ggplot2" 
library(ggplot2)

# Probaremos observar varias gráficas 


###  Gráfica de barras ###

# Esta gráfica representa la cantidad de veces que aparecen las entidades en la tabla de datos
# esta grpafica la podemos interpreatar como la cantidad de casillas por entidad 

ggplot(data=datos) +
  geom_bar(mapping = aes(x = ENTIDAD))+
  coord_flip()

ggplot(data = Resumen_datos) +
  geom_bar(mapping = aes(x = ENTIDAD, y = LISTA_NOMNAL),stat = "identity") +
  coord_flip()

ggplot(data = Resumen_datos) +
  geom_bar(mapping = aes(x = ENTIDAD, y = VOTOS),stat = "identity") +
  coord_flip()

ggplot(data = Resumen_datos) +
  geom_bar(mapping = aes(x = ENTIDAD, y = OPCION_1),stat = "identity") +
  coord_flip()

ggplot(data = Resumen_datos) +
  geom_bar(mapping = aes(x = ENTIDAD, y = OPCION_2),stat = "identity") +
  coord_flip()

ggplot(data = Resumen_datos) +
  geom_bar(mapping = aes(x = ENTIDAD, y = OPCION_3),stat = "identity") +
  coord_flip()

# De aquí podemos observat que hay tres estados en los cuales se utilizo más casilla para
# el proceso de revocacion, seria interesante comparar estos estados con su lista nominañ 

Resumen_datos <- read.csv("Revocación de mandato 2022.csv")

ggplot(data = Resumen_datos) +
  geom_point(mapping = aes(x = ENTIDAD, y = LISTA_NOMNAL )) +
  coord_flip()

# Veamos si se puede juntar estos dos gráficos para observar con más facilidad sus relaciones   


