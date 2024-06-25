# Primero me quedo con la parte de los datos que me interesa. En este caso, del 
# data frame Líneas de construcción me interesan los datos de la Ciudad de la 
# Costa y La Barra. Luego emprolijo los data frame.

###############################################################################
# Cargando datos crudos
lineas_construccion <- read.table("datos crudos/Líneas de construcción.csv"
                           , sep = ",")
# Filtrando para Canelones
lc_canelones <- lineas_construccion[lineas_construccion$V2 == "A",]
write.table(lc_canelones, file = "datos procesados/Líneas de construcción_Canelones.csv", 
            row.names = FALSE, col.name = FALSE,
            sep = ",") # creación data frame Canelones

# Filtrando para Ciudad de la Costa
# CiudadCosta <- lc_canelones[lc_canelones$V3 == "WA", -c(2)] #qué hacer con los NA?
# library(data.table)
# CiudadCosta <- data.table(lc_canelones[lc_canelones$V3 == "WA",])

# esto soluciona el problema de los NA:
CiudadCosta <- with(lc_canelones, lc_canelones[which(lc_canelones$V3 == "WA"),])


# Filtrando para Maldonado
lc_maldonado <- lineas_construccion[lineas_construccion$V2 == "A",]
write.table(lc_maldonado, file = "datos procesados/Líneas de construcción_Canelones.csv", 
            row.names = FALSE, col.name = FALSE,
            sep = ",") # creación data frame Maldonado
# Filtrando para La Barra
LaBarra <- with(lc_maldonado,lc_maldonado[which(lc_maldonado$V3 == "EH"),])


# Modificando el nombre a las columnas.

names(LaBarra) = c("Código régimen", "Código departamento", "Código localidad",
                 "Padrón", "Bloc / Manzana", "EP / SS", "Unidad", "Nivel",
                 "Código destino", "Categoría construcción", "Estado conservación",
                 "Tipo de cubierta","Indicador cielorraso", "Tipo de obra",
                 "Area construida", "Año de construcción", "Año remanente",
                 "EP/SS uso exclusivo", "Unidad uso explusivo")
names(CiudadCosta) = c("Código régimen", "Código departamento", "Código localidad",
                       "Padrón", "Bloc / Manzana", "EP / SS", "Unidad", "Nivel",
                       "Código destino", "Categoría construcción", "Estado conservación",
                       "Tipo de cubierta","Indicador cielorraso", "Tipo de obra",
                       "Area construida", "Año de construcción", "Año remanente",
                       "EP/SS uso exclusivo", "Unidad uso explusivo")

################################################################################
# Categorías de construcción casos intermedios
CatCot <- read.table("datos crudos/Categorías de Construcción.csv", header = FALSE,
                     sep = ",")
View(CatCot)
CatCot[2,2] <- "1.5"
CatCot[4,2] <- "2.5"
CatCot[6,2] <- "3.5"
CatCot[8,2] <- "4.5"

write.table(CatCot, file = "datos procesados/Categorías de Construcción.csv",
            row.names = FALSE, col.names = FALSE, sep = ",")

################################################################################

library(dplyr)

modifica <- function(dataframe, archivo, campo_clave, variable) {
  datos <- read.table(archivo, sep = ",", header = FALSE, stringsAsFactors = FALSE)
  names(datos) <- c("Clave", "Valor")
  dataframe <- left_join(dataframe, datos, by= setNames("Clave", campo_clave))
  dataframe <- select(dataframe, -all_of(campo_clave))
  names(dataframe)[names(dataframe) == "Valor"] <- variable
  
  return(dataframe)
}
str(CiudadCosta)
str(LaBarra)

CiudadCosta <- modifica(CiudadCosta,"datos crudos/Destinos.csv", "Código destino", "Destinos")
CiudadCosta <- modifica(CiudadCosta,"datos procesados/Categorías de Construcción.csv", "Categoría construcción", "Categoría de construcción")
CiudadCosta <- modifica(CiudadCosta,"datos crudos/Estados de Conservación.csv", "Estado conservación", "Estado conservación")
CiudadCosta <- modifica(CiudadCosta,"datos crudos/Cubiertas.csv", "Tipo de cubierta", "Tipo de cubierta")
CiudadCosta <- modifica(CiudadCosta,"datos crudos/Cielorrasos.csv", "Indicador cielorraso", "Indicador cielorraso")
CiudadCosta <- modifica(CiudadCosta,"datos crudos/Tipos de Obra.csv", "Tipo de obra", "Tipo de obra")
LaBarra <- modifica(LaBarra,"datos crudos/Destinos.csv", "Código destino", "Destinos")
LaBarra <- modifica(LaBarra,"datos procesados/Categorías de Construcción.csv", "Categoría construcción", "Categoría de construcción")
LaBarra <- modifica(LaBarra,"datos crudos/Estados de Conservación.csv", "Estado conservación","Estado conservación" )
LaBarra <- modifica(LaBarra,"datos crudos/Cubiertas.csv", "Tipo de cubierta","Tipo de cubierta")
LaBarra <- modifica(LaBarra,"datos crudos/Cielorrasos.csv", "Indicador cielorraso","Indicador cielorraso" )
LaBarra <- modifica(LaBarra,"datos crudos/Tipos de Obra.csv", "Tipo de obra","Tipo de obra" )


View(LaBarra)
View(CiudadCosta)

# Exportar los data frame
write.table(LaBarra, file = "datos procesados/Líneas de construcción_La barra2.csv",
            row.names = FALSE, col.names = TRUE, sep = ",")
write.table(CiudadCosta, file = "datos procesados/Líneas de construcción_Ciudad de la Costa2.csv", 
            row.names = FALSE, col.name = TRUE,
            sep = ",")

