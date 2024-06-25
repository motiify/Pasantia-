# Paquetes utilizados
library(dplyr)
library(ggplot2)
library(scales) #función percent.
library(patchwork) #combinar distintos gráficos


# Datos
CiudadCosta <- read.table("datos procesados/Líneas de construcción_Ciudad de la Costa2.csv",
                      header = TRUE, sep = ",")
CiudadCosta_CO <- CiudadCosta[CiudadCosta$Código.régimen == "CO",]
CiudadCosta_PH <- CiudadCosta[CiudadCosta$Código.régimen == "PH",]

View(CiudadCosta_PH)


# Area construida por categoria de construccion
CiudadCosta_CatCon_area_CO <- aggregate(Area.construida ~ Categoría.de.construcción , data = CiudadCosta_CO, FUN = sum)
CiudadCosta_CatCon_area_CO$Regimen <- "CO"
CiudadCosta_CatCon_area_CO$Porcentaje_AreaTotal <- CiudadCosta_CatCon_area_CO$Area.construida*100
CiudadCosta_CatCon_area_CO$Porcentaje_AreaTotal <- CiudadCosta_CatCon_area_CO$Area.construida/sum(CiudadCosta_CatCon_area_CO$Area.construida)
CiudadCosta_CatCon_area_CO$Porcentaje_AreaTotal <- percent(CiudadCosta_CatCon_area_CO$Porcentaje_AreaTotal)
CiudadCosta_CatCon_area_CO$Categoría.de.construcción <- factor(CiudadCosta_CatCon_area_CO$Categoría.de.construcción,
                                                           levels = c("Muy economica","4.5", "Economica","3.5", "Comun","2.5", "Confortable","1.5", "Muy confortable"))
CiudadCosta_CatCon_area_CO <- CiudadCosta_CatCon_area_CO[order(CiudadCosta_CatCon_area_CO$Categoría.de.construcción),]


CiudadCosta_CatCon_area_PH <- aggregate(Area.construida ~ Categoría.de.construcción , data = CiudadCosta_PH, FUN = sum)
CiudadCosta_CatCon_area_PH$Regimen <- "PH"
CiudadCosta_CatCon_area_PH$Porcentaje_AreaTotal <- CiudadCosta_CatCon_area_PH$Area.construida*100
CiudadCosta_CatCon_area_PH$Porcentaje_AreaTotal <- CiudadCosta_CatCon_area_PH$Area.construida/sum(CiudadCosta_CatCon_area_PH$Area.construida)
CiudadCosta_CatCon_area_PH$Porcentaje_AreaTotal <- percent(CiudadCosta_CatCon_area_PH$Porcentaje_AreaTotal)
CiudadCosta_CatCon_area_PH$Categoría.de.construcción <- factor(CiudadCosta_CatCon_area_PH$Categoría.de.construcción,
                                                           levels = c("Muy economica","4.5", "Economica","3.5", "Comun","2.5", "Confortable","1.5", "Muy confortable"))
CiudadCosta_CatCon_area_PH <- CiudadCosta_CatCon_area_PH[order(CiudadCosta_CatCon_area_PH$Categoría.de.construcción),]

CatCon_CO_PH <- rbind(CiudadCosta_CatCon_area_CO,CiudadCosta_CatCon_area_PH)

CatCon_COPH <- ggplot(CatCon_CO_PH, aes(x = Categoría.de.construcción, y = Area.construida, fill = Regimen)) + 
  geom_bar(stat = "identity")+    #crear un gráfico de barras apiladas para múltiples variables
  ylab("Área construida")+
  xlab("Categoría de construcción")



# Area construida por Estado de conservación
CiudadCosta_Estado_area_CO <- aggregate(Area.construida ~ Estado.conservación , data = CiudadCosta_CO, FUN = sum)
CiudadCosta_Estado_area_CO$Regimen <- "CO"
CiudadCosta_Estado_area_CO$Porcentaje_AreaTotal <- CiudadCosta_Estado_area_CO$Area.construida*100
CiudadCosta_Estado_area_CO$Porcentaje_AreaTotal <- CiudadCosta_Estado_area_CO$Area.construida/sum(CiudadCosta_Estado_area_CO$Area.construida)
CiudadCosta_Estado_area_CO$Porcentaje_AreaTotal <- percent(CiudadCosta_Estado_area_CO$Porcentaje_AreaTotal)
CiudadCosta_Estado_area_CO$Estado.conservación <- factor(CiudadCosta_Estado_area_CO$Estado.conservación,
                                                     levels = c("Excelente","Excelente/Bueno","Bueno","Bueno/Regular","Regular","Regular/Malo","Malo","Malo/Muy Malo" ,"Muy Malo","NA"))
CiudadCosta_Estado_area_CO <- CiudadCosta_Estado_area_CO[order(CiudadCosta_Estado_area_CO$Estado.conservación),]


CiudadCosta_Estado_area_PH <- aggregate(Area.construida ~ Estado.conservación , data = CiudadCosta_PH, FUN = sum)
CiudadCosta_Estado_area_PH$Regimen <- "PH"
CiudadCosta_Estado_area_PH$Porcentaje_AreaTotal <- CiudadCosta_Estado_area_PH$Area.construida*100
CiudadCosta_Estado_area_PH$Porcentaje_AreaTotal <- CiudadCosta_Estado_area_PH$Area.construida/sum(CiudadCosta_Estado_area_PH$Area.construida)
CiudadCosta_Estado_area_PH$Porcentaje_AreaTotal <- percent(CiudadCosta_Estado_area_PH$Porcentaje_AreaTotal)
CiudadCosta_Estado_area_PH$Estado.conservación <- factor(CiudadCosta_Estado_area_PH$Estado.conservación,
                                                     levels = c("Excelente","Excelente/Bueno","Bueno","Bueno/Regular","Regular","Regular/Malo","Malo","Malo/Muy Malo" ,"Muy Malo","NA"))
CiudadCosta_Estado_area_PH <- CiudadCosta_Estado_area_PH[order(CiudadCosta_Estado_area_PH$Estado.conservación),]

Estado_CO_PH <- rbind(CiudadCosta_Estado_area_CO,CiudadCosta_Estado_area_PH)

Estado_COPH <- ggplot(Estado_CO_PH, aes(x = Estado.conservación, y = Area.construida, fill = Regimen)) + 
  geom_bar(stat = "identity")+    #crear un gráfico de barras apiladas para múltiples variables
  ylab("Área construida")+
  xlab("Estado de conservación")



# Area construida por Destinos
CiudadCosta_Destino_area_CO <- aggregate(Area.construida ~ Destinos , data = CiudadCosta_CO, FUN = sum)
CiudadCosta_Destino_area_CO$Regimen <- "CO"
CiudadCosta_Destino_area_CO$Porcentaje_AreaTotal <- CiudadCosta_Destino_area_CO$Area.construida*100
CiudadCosta_Destino_area_CO$Porcentaje_AreaTotal <- CiudadCosta_Destino_area_CO$Area.construida/sum(CiudadCosta_Destino_area_CO$Area.construida)
CiudadCosta_Destino_area_CO$Porcentaje_AreaTotal <- percent(CiudadCosta_Destino_area_CO$Porcentaje_AreaTotal)

CiudadCosta_Destino_area_PH <- aggregate(Area.construida ~ Destinos , data = CiudadCosta_PH, FUN = sum)
CiudadCosta_Destino_area_PH$Regimen <- "PH"
CiudadCosta_Destino_area_PH$Porcentaje_AreaTotal <- CiudadCosta_Destino_area_PH$Area.construida*100
CiudadCosta_Destino_area_PH$Porcentaje_AreaTotal <- CiudadCosta_Destino_area_PH$Area.construida/sum(CiudadCosta_Destino_area_PH$Area.construida)
CiudadCosta_Destino_area_PH$Porcentaje_AreaTotal <- percent(CiudadCosta_Destino_area_PH$Porcentaje_AreaTotal)

Destino_CO_PH <- rbind(CiudadCosta_Destino_area_CO,CiudadCosta_Destino_area_PH)
Destino_CO_PH_mod <- Destino_CO_PH[Destino_CO_PH$Porcentaje_AreaTotal > 1,]
View(Destino_CO_PH_mod)
Destino_COPH <- ggplot(Destino_CO_PH_mod, aes(x = Destinos, y = Area.construida, fill = Regimen)) + 
  geom_bar(stat = "identity")+    #crear un gráfico de barras apiladas para múltiples variables
  ylab("Área construida")+
  xlab("Destino de conservación")+
  scale_y_continuous(breaks = seq(from = 0, to = 3600000, by = 600000))




# fil1 <- (Estado_COPH | CatCon_COPH) + plot_layout(widths = c(1,1))
# fil2 <- (plot_spacer() | Destino_COPH |plot_spacer()) + plot_layout(widths = c(0.20,0.6,0.20))

grafico <- Estado_COPH + CatCon_COPH + Destino_COPH + guide_area() + plot_layout(guides = 'collect',axis_titles = "collect")

graf_comb <- grafico  + plot_annotation(title = "Ciudad de la Costa", subtitle = "Área construida vs. Régimen") & 
  theme(axis.text.x = element_text(size = 3.5),
        axis.text.y=element_text(angle = 90, vjust = 1, hjust = 0.5, size = 3.5),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title = element_text(color = "black",
                                  hjust = 0, 
                                  size = 12, 
                                  lineheight = 1.2),
        plot.subtitle = element_text(color = "black",
                                     hjust = 0, 
                                     size = 10, 
                                     lineheight = 1.2))
graf_comb

ggsave(filename = "CiudadCosta_Regimen_COvsPH.png", plot = graf_comb, device = "png", 
       path = "salidas/Figuras/Combinadas", width = 250, height = 150, units = "mm", 
       dpi = 500, limitsize = TRUE)
