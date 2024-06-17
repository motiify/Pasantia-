# Primero me quedo con la parte de los datos que me interesa. En este caso, del 
# data frame Líneas de construcción me interesan los datos de la Ciudad de la 
# Costa y La Barra. Luego emprolijo los data frame.

###############################################################################
# Cargando datos crudos
lineas_construccion <- read.table("datos crudos/Líneas de construcción.csv"
                           , sep = ",")
# Filtrando para Canelones
lc_canelones <- lineas_construccion[lineas_construccion$V2 == "A",]
str(lc_canelones)
write.table(lc_canelones, file = "datos procesados/Líneas de construcción_Canelones.csv", 
            row.names = FALSE, col.name = FALSE,
            sep = ",") # creación data frame Canelones

# Filtrando para Ciudad de la Costa

# CiudadCosta <- lc_canelones[lc_canelones$V3 == "WA", -c(2)] #qué hacer con los NA?
# library(data.table)
# CiudadCosta <- data.table(lc_canelones[lc_canelones$V3 == "WA",])

# esto soluciona el problema de los NA: 
CiudadCosta <- with(lc_canelones, lc_canelones[which(lc_canelones$V3 == "WA"),])
str(lc_CiudadCosta)
View(lc_CiudadCosta)


# Filtrando para Maldonado
lc_maldonado <- lineas_construccion[lineas_construccion$V2 == "A",]
str(lc_maldonado)
write.table(lc_maldonado, file = "datos procesados/Líneas de construcción_Canelones.csv", 
            row.names = FALSE, col.name = FALSE,
            sep = ",") # creación data frame Maldonado
# Filtrando para La Barra
LaBarra <- with(lc_maldonado,lc_maldonado[which(lc_maldonado$V3 == "EH"),])

View(lc_barra)

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

# CAMBIANDO CÓDIGOS POR CATEGORÍAS.
# Destinos
install.packages('dplyr')
library(dplyr)
destinos <- read.table("datos crudos/Destinos.csv", header = FALSE,
                       sep = ",",)
View(destinos)
names(destinos) <- c("Código destino", "Destino")

lc_barra2 <- inner_join(lc_barra,destinos, "Código destino")
View(lc_barra2)
lc_barra2 <- lc_barra2[,-9] 

# Categoría de construcción
construccion <- read.table("datos crudos/Categorías de Construcción.csv", header = FALSE,
                       sep = ",",)
View(construccion)
names(construccion) <- c("Categoría construcción", "Denominación")

lc_barra3 <- inner_join(lc_barra2,construccion, "Categoría construcción")
View(lc_barra3)
lc_barra3 <- lc_barra3[,-9]
lc_barra3 <- rename(lc_barra3, "Categoría construcción" = Denominación)

#Estado conservación
conservacion <- read.table("datos crudos/Estados de Conservación.csv", sep = ",")
View(conservacion)
names(conservacion) <- c("Estado conservación", "Denominación")
lc_barra4 <- left_join(lc_barra3,conservacion, "Estado conservación")
View(lc_barra4)
lc_barra4 <- lc_barra4[,-9]
lc_barra4 <- rename(lc_barra4, "Estado conservación" = Denominación)

################################################################################
# usar left_join para no perder datos del data frame de líneas de construcción
################################################################################

# Tipo de cubierta
cubierta <- read.table("datos crudos/Cubiertas.csv", sep = ",")
View(cubierta)
names(cubierta) <- c("Tipo de cubierta", "Cubierta")
lc_barra5 <- left_join(lc_barra4,cubierta, "Tipo de cubierta")
View(lc_barra5)
lc_barra5 <- lc_barra5[,-9]
lc_barra5 <- rename(lc_barra5, "Tipo de cubierta" = Cubierta)

#Cielorraso
cielorraso <- read.table("datos crudos/Cielorrasos.csv", sep = ",")
View(cielorraso)
names(cielorraso) <- c("Indicador cielorraso", "Cielorraso")
lc_barra6 <- left_join(lc_barra5,cielorraso, "Indicador cielorraso")
# no hay datos
View(lc_barra6)
lc_barra6 <- lc_barra6[,-9]
lc_barra6 <- rename(lc_barra5, "Indicador cielorraso" = Cielorraso)

#Tipo de obra
Obra <- read.table("datos crudos/Tipos de Obra.csv", sep = ",")
View(obra)
names(Obra) <- c("Tipo de obra", "Denominacion")
lc_barra7 <- left_join(lc_barra5,Obra, "Tipo de obra")
View(lc_barra7)
lc_barra7 <- lc_barra7[,-10]
lc_barra7 <- rename(lc_barra7, "Tipo de obra" = Denominacion)

#################################
##RESUMIENDO ESTO EN UNA FUNCIÓN
#################################
mod <- function(dataframe, archivo, campo_clave, variable) {
  # Cargar datos del archivo CSV
  datos <- read.table(archivo, sep = ",", header = FALSE, stringsAsFactors = FALSE)
  
  # Asignar nombres a las columnas
  names(datos) <- c("Clave", "Valor")
  
  # Unir los datos
  dataframe <- left_join(dataframe, datos, by= setNames("Clave", campo_clave))
  
  #Eliminar el campo clave
  dataframe <- select(dataframe, -all_of(campo_clave))
  
  
  #Cambiar el nombre de la columna Valor
  names(dataframe)[names(dataframe) == "Valor"] <- variable
  
  return(dataframe)
}
str(CiudadCosta)
str(LaBarra)
CiudadCosta <- mod(CiudadCosta,"datos crudos/Destinos.csv", "Código destino", "Destinos")
CiudadCosta <- mod(CiudadCosta,"datos crudos/Categorías de Construcción.csv", "Categoría construcción", "Categoría de construcción")
CiudadCosta <- mod(CiudadCosta,"datos crudos/Estados de Conservación.csv", "Estado conservación", "Estado Conservación")
CiudadCosta <- mod(CiudadCosta,"datos crudos/Cubiertas.csv", "Tipo de cubierta", "Tipo de cubierta")
CiudadCosta <- mod(CiudadCosta,"datos crudos/Cielorrasos.csv", "Indicador cielorraso", "Indicador cielorraso")
CiudadCosta <- mod(CiudadCosta,"datos crudos/Tipos de Obra.csv", "Tipo de obra", "Tipo de obra")
LaBarra <- mod(LaBarra,"datos crudos/Destinos.csv", "Código destino", "Destinos")
LaBarra <- mod(LaBarra,"datos crudos/Categorías de Construcción.csv", "Categoría construcción", "Categoría de construcción")
LaBarra <- mod(LaBarra,"datos crudos/Estados de Conservación.csv", "Estado conservación","Estado conservación" )
LaBarra <- mod(LaBarra,"datos crudos/Cubiertas.csv", "Tipo de cubierta","Tipo de cubierta")
LaBarra <- mod(LaBarra,"datos crudos/Cielorrasos.csv", "Indicador cielorraso","Indicador cielorraso" )
LaBarra <- mod(LaBarra,"datos crudos/Tipos de Obra.csv", "Tipo de obra","Tipo de obra" )

View(LaBarra)
View(CiudadCosta)

# Exportar los data frame
write.table(LaBarra, file = "datos procesados/Líneas de construcción_La barra.csv",
            row.names = FALSE, col.names = TRUE, sep = ",")
write.table(CiudadCosta, file = "datos procesados/Líneas de construcción_Ciudad de la Costa.csv", 
            row.names = FALSE, col.name = TRUE,
            sep = ",")

