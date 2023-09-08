##### IMPORTACION DE DATOS  ####

# Para hacer la importacion de datos, utilizaremos la libreria "readr" de la paqueteria de Tidyverse

library(readr)

# Usaremos la funcion "read_csv" para leer el archivo 20220411_1845_COMPUTOS_RM2022.csv" que se encuentra en la carpeta de datos. 

datos <- read.csv("Datos/20220411_1845_COMPUTOS_RM2022.csv", skip = 5)

# "read_csv" traslada la infomación del archivo "20220411_1845_COMPUTOS_RM2022.csv" en un dataframe en R.
# El argumento "skip" comenzara a leer los datos a patir de la fila 5. 


##### VISUALIZACIÓN DE DATOS EN TIBLES ####

# Otra forma de visualizar los data frames es utilizando "tibles". Para poder visualizar datos en tablas tibles necesitamos la libreria tibble

library(tibble)

# Para converite el data frame de datos a un tibble usamos la función "as_tibble"

as_tibble(datos)

# Observamos que la informacion de nuestro tibble contiene datos desordenados
# El tibble muestra 22 columnas las cuales contienen diferentes variables como: 
# clave de casilla, clave de acta, Id de entidad, Distrito, Sección, Id de casilla, Tipo de casilla, Extensión contigua, Ubicación, etc.
# Las cuales con tienen datos de tipo entero y caracteres

# El tibble tambien muestra 57 449 observaciones 


#### TRANSFORMACION DE DATOS ####

# Ahora ordenaremos los datos para ello quitaremo algunas variables que no son de nuestro interes y resumiremos algunos valores. 
# Para ello utilzando la libreria "dplyr"

library(dplyr)


# primero quitaremos las varibales que no son de interes,
# Para eso, utilizaremos la función "select"


datos2 <- select(datos, c(3,4,13:17))
as_tibble(datos2)

# Ahora agruparemos la informacion de cada opción de voto, total de votos por casilla y lista nomnial por entidad 
# Para agrupar los datos utilizamos las funcionces "group_by" y "summarise" 

datos3 <- datos2 %>% group_by(ID_ENTIDAD, ENTIDAD) %>%  
  summarize(VOTOS = sum(TOTAL_VOTOS_CALCULADOS), OPCION_1 = sum(QUE_SE_LE_REVOQUE_EL_MANDATO_POR_PÉRDIDA_DE_LA_CONFIANZA), OPCION_2 = sum(QUE_SIGA_EN_LA_PRESIDENCIA_DE_LA_REPÚBLICA), OPCION_3 = sum(NULOS), LISTA_NOMNAL = sum(LISTA_NOMINAL))

# En esta parte para simplificar el nombre de las variables
# La OPCIÓN 01: son los votos contados de la opcion que se le revoque el mandato por perdida de confianza
# La OPCIÓN 02: son los vatos contados de la opcion que siga en la presidencia
# La OPCIÓN 03: son los votos nulos contados


as_tibble(datos3)


# Ahora guardaremos la tabla para poder trabajar con ella
# Para ello utilizamos la función "write_csv"

write.csv(datos3, "Revocación de mandato 2022.csv")
