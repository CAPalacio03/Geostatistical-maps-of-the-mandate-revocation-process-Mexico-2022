                           ## Mapa Geoespacial ##


library(dplyr)
library(ggplot2)
library(pacman)
library(rgdal)
library(tidyverse)

pacman::p_load(raster, rgdal,
               rgeos, stringr, sf,
               tidyverse, RColorBrewer, 
               cowplot, ggpubr, ggspatial, 
               rnaturalearth, rnaturalearthdata,
               viridisLite, viridis)

resum <- read.csv("Resumen de datos.csv")

est <- st_read("Estados/México_Estados.shp")
est <- est %>%  separate(CODIGO, c("BORRAR", "ID_ENTIDAD"), sep=2)

est <- est %>%  select_(c(2,4))

## modificando el ID

est$ID_ENTIDAD[est$ID_ENTIDAD == "01"] <-1 
est$ID_ENTIDAD[est$ID_ENTIDAD == "02"] <-2
est$ID_ENTIDAD[est$ID_ENTIDAD == "03"] <-3
est$ID_ENTIDAD[est$ID_ENTIDAD == "04"] <-4
est$ID_ENTIDAD[est$ID_ENTIDAD == "05"] <-5
est$ID_ENTIDAD[est$ID_ENTIDAD == "06"] <-6
est$ID_ENTIDAD[est$ID_ENTIDAD == "07"] <-7
est$ID_ENTIDAD[est$ID_ENTIDAD == "08"] <-8
est$ID_ENTIDAD[est$ID_ENTIDAD == "09"] <-9

est$ID_ENTIDAD <- as.numeric(est$ID_ENTIDAD)

## juntamos las base datos 

base1 <- est %>%  left_join(resum)

# Construyendo mapas de puntos

cds <- st_read("Cuidades/México_Ciudades.shp")

cds <- cds %>%  filter(CAPITAL == "C" | CAPITAL == "S")

cds$ESTADO[cds$ESTADO == "aguascalientes"] <- "AGUASCALIENTE"
cds$ESTADO[cds$ESTADO == "aguascalientes"] <- "AGUASCALIENTES"
cds$ESTADO[cds$ESTADO == "Aguascalientes"] <- "AGUASCALIENTES"
cds$ESTADO[cds$ESTADO == "Aguascalientes"]   <- "AGUASCALIENTES"
cds$ESTADO[cds$ESTADO == "Baja California"]  <- "BAJA CALIFORNIA"
cds$ESTADO[cds$ESTADO == "Baja California Sur"] <- "BAJA CALIFORNIA SUR"
cds$ESTADO[cds$ESTADO == "Campeche"]         <- "CAMPECHE"
cds$ESTADO[cds$ESTADO == "Chiapas"]          <- "CHIAPAS"
cds$ESTADO[cds$ESTADO == "Chihuahua"]        <- "CHIHUAHUA"
cds$ESTADO[cds$ESTADO == "Coahuila"]         <- "COAHUILA"
cds$ESTADO[cds$ESTADO == "Colima"]           <- "COLIMA"
cds$ESTADO[cds$ESTADO == "Distrito Federal"] <- "CIUDAD DE MÉXICO"
cds$ESTADO[cds$ESTADO == "Durango"]          <- "DURANGO"
cds$ESTADO[cds$ESTADO == "Guanajuato"]       <- "GUANAJUATO"
cds$ESTADO[cds$ESTADO == "Guerrero"]         <- "GUERRERO"
cds$ESTADO[cds$ESTADO == "Hidalgo"]          <- "HIDALGO"
cds$ESTADO[cds$ESTADO == "Jalisco"]          <- "JALISCO"
cds$ESTADO[cds$ESTADO == "México"]           <- "MÉXICO"
cds$ESTADO[cds$ESTADO == "Michoacán"]        <- "MICHOACÁN"
cds$ESTADO[cds$ESTADO == "Morelos"]          <- "MORELOS"
cds$ESTADO[cds$ESTADO == "Nayarit"]          <- "NAYARIT"
cds$ESTADO[cds$ESTADO == "Nuevo León"]       <- "NUEVO LEÓN"
cds$ESTADO[cds$ESTADO == "Oaxaca"]           <- "OAXACA"
cds$ESTADO[cds$ESTADO == "Puebla"]           <- "PUEBLA"
cds$ESTADO[cds$ESTADO == "Querétaro"]        <- "QUERÉTARO"
cds$ESTADO[cds$ESTADO == "Quintana Roo"]     <- "QUINTANA ROO"
cds$ESTADO[cds$ESTADO == "San Luis Potosí"]  <- "SAN LUIS POTOSÍ"
cds$ESTADO[cds$ESTADO == "Sinaloa"]          <- "SINALOA"
cds$ESTADO[cds$ESTADO == "Sonora"]           <- "SONORA"
cds$ESTADO[cds$ESTADO == "Tabasco"]          <- "TABASCO"
cds$ESTADO[cds$ESTADO == "Tamaulipas"]       <- "TAMAULIPAS"
cds$ESTADO[cds$ESTADO == "Tlaxcala"]         <- "TLAXCALA"
cds$ESTADO[cds$ESTADO == "Veracruz"]         <- "VERACRUZ" 
cds$ESTADO[cds$ESTADO == "Yucatán"]          <- "YUCATÁN"
cds$ESTADO[cds$ESTADO == "Zacatecas"]        <- "ZACATECAS"

cds <- cds %>%  mutate(ENTIDAD = ESTADO)

cds <- cds %>%  select_(4,5)


base2 <- cds %>%  left_join(resum)

## haciendo el mapa

ggplot() +
  geom_sf(data = base1, aes(fill = OPCION_2)) +
  scale_fill_viridis_b(direction = -1) +
  geom_sf(data = base2, aes(size = Participación), alpha = 0.3, color = 'red') +
  labs(title = "RESULTADOS DE LA \n REVOCACIÓN DE MANDATO 2022",
       caption = "Fuente: INE",
       fill = "Votos a favor de que\n siga en el puesto",
       size = "Participación cuidadana \n medida en porcentaje") +
  theme_bw() + 
  theme(panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill =  "white"),
        panel.grid =  element_blank(),
        plot.title = element_text(hjust = 0.5, size = 15),
        legend.background = element_rect(fill = NA)) 


## Otro mapa

ggplot() +
  geom_sf(data = base1, aes(fill = OPCION_2)) +
  scale_fill_distiller(palette = "Greens", direction = 1) +
  geom_sf(data = base2, aes(size = Participación), alpha = 0.3 , color= "red") +
  labs(title = "TASA DE VOTOS",
       caption = "Fuente: INE",
       fill = "Votos a favor de que \n siga en el puesto",
       size = "Particapacion cuidadana") +
  theme_bw() + 
  theme(panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill =  "white"),
        panel.grid =  element_blank())
  
## Mapa de participación cuidadna
ggplot() +
  geom_sf(data = base1, aes(fill = Participación)) +
  scale_fill_distiller(palette = "Reds", direction = 1) +
  labs(title = "PARTICIPACIÓN CUIDADANA EN EL PROCESO DE \n REVOCACIÓN DE MANDATO 2022",
       caption = "Fuente: INE",
       fill = "Participación cuidadana \n en porcentaje") +
  theme_bw() + 
  theme(panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill =  "white"),
        panel.grid =  element_blank(),
        plot.title = element_text(hjust = 0.5, size = 15),
        legend.background = element_rect(fill = NA)) 

# Mapa con el número de votos registrados por entidad federativa 

ggplot() +
  geom_sf(data = base1, aes(fill = VOTOS)) +
  scale_fill_distiller(direction = 1) +
  labs(title = "NÚMERO DE VOTOS EN EL PROCESO DE \n REVOCACIÓN DE MANDATO 2022",
       caption = "Fuente: INE",
       fill = "Cantidad de votos de \n registrados por Estado") +
  theme_bw() + 
  theme(panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill =  "white"),
        panel.grid =  element_blank(),
        plot.title = element_text(hjust = 0.5, size = 15),
        legend.background = element_rect(fill = NA)) 

# Mapa con el número de cuidadanos disponibles para votar (Lista Nominal)

ggplot() +
  geom_sf(data = base1, aes(fill = LISTA_NOMNAL)) +
  scale_fill_distiller(palette = "Reds", direction = 1) +
  labs(title = "MAPA DE LISTA NOMINAL DE CADA ENTIDAD \n  DURANRE LA REVOCACIÓN DE MANDATO 2022",
       caption = "Fuente: INE",
       fill = "Cantidad de cuidadanos \n con capacidad de votar") +
  theme_bw() + 
  theme(panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill =  "white"),
        panel.grid =  element_blank(),
        plot.title = element_text(hjust = 0.5, size = 15),
        legend.background = element_rect(fill = NA)) 

