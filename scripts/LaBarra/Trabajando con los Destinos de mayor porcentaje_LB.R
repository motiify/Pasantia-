# Paquetes utilizados
library(dplyr)
library(ggplot2)
library(scales) #función percent.
library(patchwork) #combinar distintos gráficos


# Datos
LaBarra <- read.table("datos procesados/Líneas de construcción_La barra2.csv",
                      header = TRUE, sep = ",")
# Area construida por Destino
LB_Dest_area <- aggregate(Area.construida ~ Destinos , data = LaBarra, FUN = sum)
LB_Dest_area$Porcentaje_AreaTotal <- LB_Dest_area$Area.construida*100
LB_Dest_area$Porcentaje_AreaTotal <- LB_Dest_area$Area.construida/sum(LB_Dest_area$Area.construida)
LB_Dest_area$Porcentaje_AreaTotal <- percent(LB_Dest_area$Porcentaje_AreaTotal)
LB_Area_ord <- LB_Dest_area[order(LB_Dest_area$Area.construida,decreasing = TRUE ),] 
LB_Area_ord[c(1,2),] # Hotel, Motel y Vivienda son los destinos

LB_filt <- LaBarra[LaBarra$Destinos == "VIVIENDA" | LaBarra$Destinos == "HOTEL, MOTEL",]
table(LB_filt$Destinos)


# CATEGORÍA DE CONSTRUCCIÓN PARA HOTEL, MOTEL Y VIVIENDA
Area <- aggregate(Area.construida ~ Categoría.de.construcción + Destinos, data = LB_filt, FUN = sum)
Area$Porcentaje <- (Area$Area.construida*100)/sum(Area$Area.construida)
Area$Categoría.de.construcción <- factor(Area$Categoría.de.construcción,
                                                  levels = c("Muy confortable","1.5","Confortable","2.5","Comun","3.5","Economica","4.5","Muy economica"))
Area <- Area[order(Area$Categoría.de.construcción),]

CatCon <- ggplot(Area, aes(x = Categoría.de.construcción, y = Porcentaje, fill = Destinos)) +
  geom_bar(stat = "identity", position = "dodge") +
  xlab("Categorías de construcción") + 
  ylab("Porcentaje de área construida") +
  labs(title = "La Barra",caption = "Para el cálculo del porcentaje se utilizó el total de área construída\n
       para los destinos Hotel, Motel y Vivienda")

# ESTADO DE CONSERVACIÓN PARA HOTEL, MOTEL Y VIVIENDA
Area2 <- aggregate(Area.construida ~ Estado.conservación + Destinos, data = LB_filt, FUN = sum)
Area2$Porcentaje <- (Area2$Area.construida*100)/sum(Area2$Area.construida)
Area2$Estado.conservación <- factor(Area2$Estado.conservación,
                                         levels = c("Excelente","Excelente/Bueno","Bueno","Bueno/Regular","Regular","Regular/Malo","Malo","Malo/Muy Malo" ,"Muy Malo","NA"))
Area2 <- Area2[order(Area2$Estado.conservación),]

Estado <- ggplot(Area2, aes(x = Estado.conservación, y = Porcentaje, fill = Destinos)) +
  geom_bar(stat = "identity", position = "dodge") +
  xlab("Estado de conservación") + 
  ylab("Porcentaje de área construida") +
  labs(title = "La Barra",caption = "Para el cálculo del porcentaje se utilizó el total de área construída\n
       para los destinos Hotel, Motel y Vivienda")

### DJCU
PadUrb_LB <- read.table("datos procesados/Padrones Urbanos_LaBarra.csv",
                        header = TRUE, sep = ",")

# Genero el identificador
PadUrb_LB$Identificador <- ifelse(PadUrb_LB$Código.Régimen == "PH" | PadUrb_LB$Código.Régimen == "UH",
                                paste(PadUrb_LB$Código.Departamento, PadUrb_LB$Código.Localidad, PadUrb_LB$Padrón, PadUrb_LB$Unidad, sep = "-"),
                                paste(PadUrb_LB$Código.Departamento, PadUrb_LB$Código.Localidad, PadUrb_LB$Padrón, sep = "-")
)


LB_filt$Identificador <- ifelse(LB_filt$Código.régimen == "PH" | LB_filt$Código.régimen == "UH",
  paste(LB_filt$Código.departamento, LB_filt$Código.localidad, LB_filt$Padrón, LB_filt$Unidad, sep = "-"),
  paste(LB_filt$Código.departamento, LB_filt$Código.localidad, LB_filt$Padrón, sep = "-")
)

# Filtramos los padrones por destino (en este caso Vivienda y Hotel, Motel)
PadLine_LB <- semi_join(PadUrb_LB, LB_filt, by = "Identificador") #filtra la capa de padrones


# Se presentó alguna vez una DJCU o no?
PadLine_LB$DeclaraciónJurada <- ifelse(PadLine_LB$`Fecha.última.DJCU` == "/  /", "No", "Si")
Area_DJCU <- aggregate(Área.edificada ~ DeclaraciónJurada + Código.Régimen , data = PadLine_LB, FUN = sum)
Area_DJCU$Porcentaje <- (Area_DJCU$Área.edificada*100)/sum(Area_DJCU$Área.edificada)

DJCU_LB <- ggplot(Area_DJCU, aes(x = DeclaraciónJurada, y = Porcentaje, fill = Código.Régimen)) +
  geom_bar(stat = "identity", position = "dodge") +
  xlab("Presenta DJCU") + 
  ylab("Porcentaje de DJCU")+
  labs(title = "La Barra")
DJCU_LB

# Tiene DJCU vigente
PadLine_LB$Vigencia.última.DJCU <- as.Date(PadLine_LB$Vigencia.última.DJCU, format = "%d/%m/%Y")
fecha_limite <- as.Date("2024-07-08") 
PadLine_LB$DJCU_Vigente <- ifelse(PadLine_LB$`Vigencia.última.DJCU` < fecha_limite, "No", "Si")
PadLine_LB["DJCU_Vigente"][is.na(PadLine_LB["DJCU_Vigente"])] <- "Nunca se presentó DJCU"

DJCU_Vigente_LB <- as.data.frame.table(table(PadLine_LB$DJCU_Vigente, PadLine_LB$Código.Régimen)) # cuenta los casos de Si/No/Nunca se presentó DJCU
DJCU_Vigente_LB$Porcentaje <- (DJCU_Vigente_LB$Freq*100)/sum(DJCU_Vigente_LB$Freq)
names(DJCU_Vigente_LB) <- c("Vigencia_DJCU","Régimen", "Cantidad","Porcentaje")
DJCU_Vigente_LB$Vigencia_DJCU <- factor(DJCU_Vigente_LB$Vigencia_DJCU, levels = c("Si", "No", "Nunca se presentó DJCU"))
DJCU_Vigente_LB <- DJCU_Vigente_LB[order(DJCU_Vigente_LB$Vigencia_DJCU),]


Vigencia_DJCU_LB <- ggplot(DJCU_Vigente_LB, aes(x = Vigencia_DJCU, y = Porcentaje, fill = Régimen)) +
  geom_bar(stat = "identity", position = "dodge") +
  xlab("DJCU vigente") + 
  ylab("Porcentaje de DJCU")+
  labs(title = "La Barra")

Vigencia_DJCU_LB


Lineas_DJCU <- left_join( PadLine_LB,LB_filt, by = "Identificador", relationship = "many-to-one")
