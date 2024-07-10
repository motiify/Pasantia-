# Paquetes utilizados
library(dplyr)
library(ggplot2)
library(scales) #función percent.
library(patchwork) #combinar distintos gráficos


# Datos
CiudadCosta <- read.table("datos procesados/Líneas de construcción_Ciudad de la Costa2.csv",
                      header = TRUE, sep = ",")
# Area construida por Destino
CC_Dest_area <- aggregate(Area.construida ~ Destinos , data = CiudadCosta, FUN = sum)
CC_Dest_area$Porcentaje_AreaTotal <- CC_Dest_area$Area.construida*100
CC_Dest_area$Porcentaje_AreaTotal <- CC_Dest_area$Area.construida/sum(CC_Dest_area$Area.construida)
CC_Dest_area$Porcentaje_AreaTotal <- percent(CC_Dest_area$Porcentaje_AreaTotal)
CC_Area_ord <- CC_Dest_area[order(CC_Dest_area$Area.construida,decreasing = TRUE ),] 
CC_Area_ord[c(1,2),] # Depósito  y Vivienda son los destinos principales

CC_filt <- CiudadCosta[CiudadCosta$Destinos == "VIVIENDA" | CiudadCosta$Destinos == "DEPOSITO",]
table(CC_filt$Destinos)


# CATEGORÍA DE CONSTRUCCIÓN PARA HOTEL, MOTEL Y VIVIENDA
Area <- aggregate(Area.construida ~ Categoría.de.construcción + Destinos, data = CC_filt, FUN = sum)
Area$Porcentaje <- (Area$Area.construida*100)/sum(Area$Area.construida)
Area$Categoría.de.construcción <- factor(Area$Categoría.de.construcción,
                                         levels = c("Muy confortable","1.5","Confortable","2.5","Comun","3.5","Economica","4.5","Muy economica"))
Area <- Area[order(Area$Categoría.de.construcción),]

CatCon <- ggplot(Area, aes(x = Categoría.de.construcción, y = Porcentaje, fill = Destinos)) +
  geom_bar(stat = "identity", position = "dodge") +
  xlab("Categorías de construcción") + 
  ylab("Porcentaje de área construida") +
  labs(caption = "Para el cálculo del porcentaje se utilizó el total de área construída\n
       para los destinos Vivienda y Depósito")

# ESTADO DE CONSERVACIÓN PARA HOTEL, MOTEL Y VIVIENDA
Area2 <- aggregate(Area.construida ~ Estado.conservación + Destinos, data = CC_filt, FUN = sum)
Area2$Porcentaje <- (Area2$Area.construida*100)/sum(Area2$Area.construida)
Area2$Estado.conservación <- factor(Area2$Estado.conservación,
                                    levels = c("Excelente","Excelente/Bueno","Bueno","Bueno/Regular","Regular","Regular/Malo","Malo","Malo/Muy Malo" ,"Muy Malo","NA"))
Area2 <- Area2[order(Area2$Estado.conservación),]

Estado <- ggplot(Area2, aes(x = Estado.conservación, y = Porcentaje, fill = Destinos)) +
  geom_bar(stat = "identity", position = "dodge") +
  xlab("Estado de conservación") + 
  ylab("Porcentaje de área construida") +
  labs(caption = "Para el cálculo del porcentaje se utilizó el total de área construída\n
       para los destinos Vivienda y Depósito")

# DJCU
PadUrb_CC <- read.table("datos procesados/Padrones Urbanos_CiudadCosta.csv",
                        header = TRUE, sep = ",")

# Genero el identificador
PadUrb_CC$Identificador <- ifelse(PadUrb_CC$Código.Régimen == "PH" | PadUrb_CC$Código.Régimen == "UH",
                                  paste(PadUrb_CC$Código.Departamento, PadUrb_CC$Código.Localidad, PadUrb_CC$Padrón, PadUrb_CC$Unidad, sep = "-"),
                                  paste(PadUrb_CC$Código.Departamento, PadUrb_CC$Código.Localidad, PadUrb_CC$Padrón, sep = "-")
)


CC_filt$Identificador <- ifelse(CC_filt$Código.régimen == "PH" | CC_filt$Código.régimen == "UH",
                                paste(CC_filt$Código.departamento, CC_filt$Código.localidad, CC_filt$Padrón, CC_filt$Unidad, sep = "-"),
                                paste(CC_filt$Código.departamento, CC_filt$Código.localidad, CC_filt$Padrón, sep = "-")
)

# Filtramos
PadLine_CC <- semi_join(PadUrb_CC, CC_filt, by = "Identificador") #filtra la capa de padrones


# Tienen DJCU o no?
PadLine_CC$DeclaraciónJurada <- ifelse(PadLine_CC$`Fecha.última.DJCU` == "/  /", "No", "Si")
Area_DJCU <- aggregate(Área.edificada ~ DeclaraciónJurada + Código.Régimen , data = PadLine_CC, FUN = sum)
Area_DJCU$Porcentaje <- (Area_DJCU$Área.edificada*100)/sum(Area_DJCU$Área.edificada)

DJCU_CC <- ggplot(Area_DJCU, aes(x = DeclaraciónJurada, y = Porcentaje, fill = Código.Régimen)) +
  geom_bar(stat = "identity", position = "dodge") +
  xlab("DJCU") + 
  ylab("Porcentaje de DJCU")+
  labs(title = "Ciudad de la Costa")
DJCU_CC

# Tiene DJCU vigente
PadLine_CC$Vigencia.última.DJCU <- as.Date(PadLine_CC$Vigencia.última.DJCU, format = "%d/%m/%Y")
fecha_limite <- as.Date("2024-07-08") 
PadLine_CC$DJCU_Vigente <- ifelse(PadLine_CC$`Vigencia.última.DJCU` < fecha_limite, "No", "Si")
table(PadLine_CC$DJCU_Vigente)
# No    Si 
# 10063 11012

DJCU_Vigente_CC <- as.data.frame.table(table(PadLine_CC$DJCU_Vigente))
DJCU_Vigente_CC$Porcentaje <- (DJCU_Vigente_CC$Freq*100)/sum(DJCU_Vigente_CC$Freq)

Vigencia_DJCU_CC <- ggplot(DJCU_Vigente_CC, aes(x = Var1, y = Porcentaje)) +
  geom_bar(stat = "identity") +
  xlab("DJCU") + 
  ylab("Porcentaje de DJCU")+
  geom_text(aes(label = Freq), vjust = -0.5,  size = 2) +
  labs(title = "Ciudad de la Costa")

Vigencia_DJCU_CC
