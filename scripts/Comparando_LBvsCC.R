# Paquetes
library(dplyr) # Calcular frecuencia
library(ggplot2) # Gráficos 
library(patchwork) # Combinar gráficos
library(scales)

# Cargar datos
LaBarra <- read.table("datos procesados/Líneas de construcción_La barra2.csv",
                      header = TRUE, sep = ",")

CiudadCosta <- read.table("datos procesados/Líneas de construcción_Ciudad de la Costa2.csv",
                      header = TRUE, sep = ",")



# RÉGIMEN
LaBarra_Reg_area <- aggregate(Area.construida ~ Código.régimen , data = LaBarra, FUN = sum)
LaBarra_Reg_area$Porcentaje_AreaTotal <- LaBarra_Reg_area$Area.construida*100
LaBarra_Reg_area$Porcentaje_AreaTotal <- LaBarra_Reg_area$Area.construida/sum(LaBarra_Reg_area$Area.construida)
LaBarra_Reg_area$Porcentaje_AreaTotal <- percent(LaBarra_Reg_area$Porcentaje_AreaTotal)
LaBarra_Reg_area$Porcentaje_AreaTotal <- as.numeric(sub("%", "", LaBarra_Reg_area$Porcentaje_AreaTotal))
LaBarra_Reg_area$Localidad <- "La Barra"

CiudadCosta_Reg_area <- aggregate(Area.construida ~ Código.régimen , data = CiudadCosta, FUN = sum)
CiudadCosta_Reg_area$Porcentaje_AreaTotal <- CiudadCosta_Reg_area$Area.construida*100
CiudadCosta_Reg_area$Porcentaje_AreaTotal <- CiudadCosta_Reg_area$Area.construida/sum(CiudadCosta_Reg_area$Area.construida)
CiudadCosta_Reg_area$Porcentaje_AreaTotal <- percent(CiudadCosta_Reg_area$Porcentaje_AreaTotal)
CiudadCosta_Reg_area$Porcentaje_AreaTotal <- as.numeric(sub("%", "", CiudadCosta_Reg_area$Porcentaje_AreaTotal))

CiudadCosta_Reg_area$Localidad <- "Ciudad de la Costa"

Reg_comb <- rbind(LaBarra_Reg_area,CiudadCosta_Reg_area)

Reg <- ggplot(Reg_comb, aes(x = Código.régimen, y = Porcentaje_AreaTotal, fill = Localidad)) + 
  geom_bar(stat = "identity", position = "dodge")+    #crear un gráfico de barras apiladas para múltiples variables
  ylab("Porcentaje área total construída")+
  xlab("Régimen")+
  theme(axis.text = element_text(size=5.5))


# CATEGORIA DE CONSTRUCCION
LaBarra_CatCon_area <- aggregate(Area.construida ~ Categoría.de.construcción , data = LaBarra, FUN = sum)
LaBarra_CatCon_area$Porcentaje_AreaTotal <- LaBarra_CatCon_area$Area.construida*100
LaBarra_CatCon_area$Porcentaje_AreaTotal <- LaBarra_CatCon_area$Area.construida/sum(LaBarra_CatCon_area$Area.construida)
LaBarra_CatCon_area$Porcentaje_AreaTotal <- percent(LaBarra_CatCon_area$Porcentaje_AreaTotal)
LaBarra_CatCon_area$Porcentaje_AreaTotal <- as.numeric(sub("%", "", LaBarra_CatCon_area$Porcentaje_AreaTotal))

LaBarra_CatCon_area$Localidad <- "La Barra"
LaBarra_CatCon_area$Categoría.de.construcción <- factor(LaBarra_CatCon_area$Categoría.de.construcción,
                                                        levels = c("Muy confortable","1.5","Confortable","2.5","Comun","3.5","Economica","4.5","Muy economica"))
LaBarra_CatCon_area <- LaBarra_CatCon_area[order(LaBarra_CatCon_area$Categoría.de.construcción),]


CiudadCosta_CatCon_area <- aggregate(Area.construida ~ Categoría.de.construcción , data = CiudadCosta, FUN = sum)
CiudadCosta_CatCon_area$Porcentaje_AreaTotal <- CiudadCosta_CatCon_area$Area.construida*100
CiudadCosta_CatCon_area$Porcentaje_AreaTotal <- CiudadCosta_CatCon_area$Area.construida/sum(CiudadCosta_CatCon_area$Area.construida)
CiudadCosta_CatCon_area$Porcentaje_AreaTotal <- percent(CiudadCosta_CatCon_area$Porcentaje_AreaTotal)
CiudadCosta_CatCon_area$Porcentaje_AreaTotal <- as.numeric(sub("%", "", CiudadCosta_CatCon_area$Porcentaje_AreaTotal))
CiudadCosta_CatCon_area$Localidad <- "Ciudad de la Costa"
CiudadCosta_CatCon_area$Categoría.de.construcción <- factor(CiudadCosta_CatCon_area$Categoría.de.construcción,
                                                             levels = c("Muy confortable","1.5","Confortable","2.5","Comun","3.5","Economica","4.5","Muy economica"))
CiudadCosta_CatCon_area <- CiudadCosta_CatCon_area[order(CiudadCosta_CatCon_area$Categoría.de.construcción),]


CatCon_comb <- rbind(LaBarra_CatCon_area,CiudadCosta_CatCon_area)

CatCon <- ggplot(CatCon_comb, aes(x = Categoría.de.construcción, y = Porcentaje_AreaTotal, fill = Localidad)) + 
  geom_bar(stat = "identity", position = "dodge")+    #crear un gráfico de barras apiladas para múltiples variables
  ylab("Porcentaje área total construída")+
  xlab("Categorías de construcción")+
  theme(axis.text = element_text(size=5.5))

# ESTADOS DE CONSERVACIÓN
LaBarra_Estado_area <- aggregate(Area.construida ~ Estado.conservación , data = LaBarra, FUN = sum)
LaBarra_Estado_area$Porcentaje_AreaTotal <- LaBarra_Estado_area$Area.construida*100
LaBarra_Estado_area$Porcentaje_AreaTotal <- LaBarra_Estado_area$Area.construida/sum(LaBarra_Estado_area$Area.construida)
LaBarra_Estado_area$Porcentaje_AreaTotal <- percent(LaBarra_Estado_area$Porcentaje_AreaTotal)
LaBarra_Estado_area$Porcentaje_AreaTotal <- as.numeric(sub("%", "", LaBarra_Estado_area$Porcentaje_AreaTotal))
LaBarra_Estado_area$Localidad <- "La Barra"
LaBarra_Estado_area$Estado.conservación <- factor(LaBarra_Estado_area$Estado.conservación,
                                                  levels = c("Excelente","Excelente/Bueno","Bueno","Bueno/Regular","Regular","Regular/Malo","Malo","Malo/Muy Malo" ,"Muy Malo","NA"))
LaBarra_Estado_area <- LaBarra_Estado_area[order(LaBarra_Estado_area$Estado.conservación),]


CiudadCosta_Estado_area <- aggregate(Area.construida ~ Estado.conservación , data = CiudadCosta, FUN = sum)
CiudadCosta_Estado_area$Porcentaje_AreaTotal <- CiudadCosta_Estado_area$Area.construida*100
CiudadCosta_Estado_area$Porcentaje_AreaTotal <- CiudadCosta_Estado_area$Area.construida/sum(CiudadCosta_Estado_area$Area.construida)
CiudadCosta_Estado_area$Porcentaje_AreaTotal <- percent(CiudadCosta_Estado_area$Porcentaje_AreaTotal)
CiudadCosta_Estado_area$Porcentaje_AreaTotal <- as.numeric(sub("%", "", CiudadCosta_Estado_area$Porcentaje_AreaTotal))
CiudadCosta_Estado_area$Localidad <- "Ciudad de la Costa"
CiudadCosta_Estado_area$Estado.conservación <- factor(CiudadCosta_Estado_area$Estado.conservación,
                                                      levels = c("Excelente","Excelente/Bueno","Bueno","Bueno/Regular","Regular","Regular/Malo","Malo","Malo/Muy Malo" ,"Muy Malo","NA"))
CiudadCosta_Estado_area <- CiudadCosta_Estado_area[order(CiudadCosta_Estado_area$Estado.conservación),]

Estados_comb <- rbind(LaBarra_Estado_area,CiudadCosta_Estado_area)

Estado <- ggplot(Estados_comb, aes(x = Estado.conservación, y = Porcentaje_AreaTotal, fill = Localidad)) + 
  geom_bar(stat = "identity", position = "dodge")+    #crear un gráfico de barras apiladas para múltiples variables
  ylab("Porcentaje área total construída") + 
  xlab("Estados de conservación")+
  theme(axis.text = element_text(size=5.5))


# DESTINOS
LaBarra_Dest_area <- aggregate(Area.construida ~ Destinos , data = LaBarra, FUN = sum)
LaBarra_Dest_area$Porcentaje_AreaTotal <- LaBarra_Dest_area$Area.construida*100
LaBarra_Dest_area$Porcentaje_AreaTotal <- LaBarra_Dest_area$Area.construida/sum(LaBarra_Dest_area$Area.construida)
LaBarra_Dest_area$Porcentaje_AreaTotal <- percent(LaBarra_Dest_area$Porcentaje_AreaTotal)
LaBarra_Dest_area$Porcentaje_AreaTotal <- as.numeric(sub("%", "", LaBarra_Dest_area$Porcentaje_AreaTotal))
LaBarra_Dest_area$Localidad <- "La Barra"
LaBarra_Dest_area_mod <- LaBarra_Dest_area[LaBarra_Dest_area$Porcentaje_AreaTotal > 1,]
LaBarra_Dest_area_mod$Destinos[3] <-"CUB-TECH-COB"
LaBarra_Dest_area_mod$Destinos[5] <-"GALE-VIV"


CiudadCosta_Dest_area <- aggregate(Area.construida ~ Destinos , data = CiudadCosta, FUN = sum)
CiudadCosta_Dest_area$Porcentaje_AreaTotal <- CiudadCosta_Dest_area$Area.construida*100
CiudadCosta_Dest_area$Porcentaje_AreaTotal <- CiudadCosta_Dest_area$Area.construida/sum(CiudadCosta_Dest_area$Area.construida)
CiudadCosta_Dest_area$Porcentaje_AreaTotal <- percent(CiudadCosta_Dest_area$Porcentaje_AreaTotal)
CiudadCosta_Dest_area$Porcentaje_AreaTotal <- as.numeric(sub("%", "", CiudadCosta_Dest_area$Porcentaje_AreaTotal))
CiudadCosta_Dest_area$Localidad <- "Ciudad de la Costa"
CiudadCosta_Dest_area_mod <- CiudadCosta_Dest_area[CiudadCosta_Dest_area$Porcentaje_AreaTotal > 1,]
CiudadCosta_Dest_area_mod$Destinos[4] <-"CUB-TECH-COB"
CiudadCosta_Dest_area_mod$Destinos[6] <-"ESCRI-ESTU-OFIC"


Destinos_comb <- rbind(LaBarra_Dest_area_mod,CiudadCosta_Dest_area_mod)

Destino <- ggplot(Destinos_comb, aes(x = Destinos, y = Porcentaje_AreaTotal, fill = Localidad)) + 
  geom_bar(stat = "identity", position = "dodge")+    #crear un gráfico de barras apiladas para múltiples variables
  ylab("Porcentaje área total construída") + 
  xlab("Destinos")+
  theme(axis.text = element_text(size=3))



graf_comb <- ((Destino + Estado + plot_layout(axis_titles = "collect")) /
  (CatCon + Reg + plot_layout(axis_titles = "collect"))) + plot_layout(guides = 'collect') 

Combinado <- graf_comb + plot_annotation(title = 'La Barra vs Ciudad de la Costa') &
  theme(axis.text.y=element_text(angle = 90, vjust = 1, hjust = 0.5, size = 6.5),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title = element_text(color = "black",
                                  hjust = 0.5,
                                  size = 14,
                                  lineheight = 1.2))

# axis.text = element_text(size=2.5),
# Exportando figuras
ggsave(filename = "CCvsLB_CategoríaConstrucción.png", plot = CatCon, device = "png", 
       path = "salidas/Figuras/Comparativas", width = 250, height = 150, units = "mm", 
       dpi = 500, limitsize = TRUE)
ggsave(filename = "CCvsLB_Régimen.png", plot = Reg, device = "png", 
       path = "salidas/Figuras/Comparativas", width = 250, height = 150, units = "mm", 
       dpi = 500, limitsize = TRUE)
ggsave(filename = "CCvsLB_EstadoConservación.png", plot = Estado, device = "png", 
       path = "salidas/Figuras/Comparativas", width = 250, height = 150, units = "mm", 
       dpi = 500, limitsize = TRUE)
ggsave(filename = "CCvsLB_Destinos.png", plot = Destino, device = "png", 
       path = "salidas/Figuras/Comparativas", width = 250, height = 150, units = "mm", 
       dpi = 500, limitsize = TRUE)


#todos los gráficos juntos
ggsave(filename = "CCvsLB.png", plot = Combinado, device = "png", 
       path = "salidas/Figuras/Comparativas", width = 250, height = 150, units = "mm", 
       dpi = 500, limitsize = TRUE)
