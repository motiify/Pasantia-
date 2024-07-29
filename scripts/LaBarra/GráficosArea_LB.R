# Paquetes utilizados
library(dplyr)
library(ggplot2)
library(scales) #función percent.
library(patchwork) #combinar distintos gráficos


# Datos
LaBarra <- read.table("datos procesados/Líneas de construcción_La barra2.csv",
                      header = TRUE, sep = ",")

# Area construida por Destino
LaBarra_Dest_area <- aggregate(Area.construida ~ Destinos , data = LaBarra, FUN = sum)
LaBarra_Dest_area$Porcentaje_AreaTotal <- LaBarra_Dest_area$Area.construida*100
LaBarra_Dest_area$Porcentaje_AreaTotal <- LaBarra_Dest_area$Area.construida/sum(LaBarra_Dest_area$Area.construida)
LaBarra_Dest_area$Porcentaje_AreaTotal <- percent(LaBarra_Dest_area$Porcentaje_AreaTotal)
LaBarra_Dest_area_mod <- LaBarra_Dest_area[LaBarra_Dest_area$Porcentaje_AreaTotal > 1,]

# Gráfico
graf_Dest <- ggplot(LaBarra_Dest_area_mod, aes(x = Destinos, y = Area.construida)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Porcentaje_AreaTotal), vjust = -0.5,  size = 2) +
  xlab("Destinos") + 
  ylab(bquote("Área construida"~(m^2))) +
  labs(caption = "Porcentajes > 1% del Área total  /  Área total: 339.868" ~ m^2)+
  theme(axis.text = element_text(size=5),
        plot.title = element_text(color = "black",
                                  hjust = 0.5, 
                                  size = 14, 
                                  lineheight = 1.2))


# Area construida por Categoria de construccion
LaBarra_CatCon_area <- aggregate(Area.construida ~ Categoría.de.construcción , data = LaBarra, FUN = sum)
LaBarra_CatCon_area$Porcentaje_AreaTotal <- LaBarra_CatCon_area$Area.construida*100
LaBarra_CatCon_area$Porcentaje_AreaTotal <- LaBarra_CatCon_area$Area.construida/sum(LaBarra_CatCon_area$Area.construida)
LaBarra_CatCon_area$Porcentaje_AreaTotal <- percent(LaBarra_CatCon_area$Porcentaje_AreaTotal)


# Ordenar
LaBarra_CatCon_area$Categoría.de.construcción <- factor(LaBarra_CatCon_area$Categoría.de.construcción,
                                                         levels = c("Muy confortable","1.5","Confortable","2.5","Comun","3.5","Economica","4.5","Muy economica"))
LaBarra_CatCon_area <- LaBarra_CatCon_area[order(LaBarra_CatCon_area$Categoría.de.construcción),]

# Gráfico
graf_CatCon <- ggplot(LaBarra_CatCon_area, aes(x = Categoría.de.construcción, y = Area.construida)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Porcentaje_AreaTotal), vjust = -0.5,  size = 2) +
  xlab("Categoría de construcción") + 
  ylab(bquote("Área construida"~(m^2))) +
  labs(caption = "Área total: 339.868" ~ m^2)+
  scale_x_discrete(labels = c("Muy confortable","1.5","Confortable","2.5","Comun","3.5","Economica","4.5","Muy economica"))+
  theme(axis.text = element_text(size=8),
        plot.title = element_text(color = "black",
                                  hjust = 0.5, 
                                  size = 14, 
                                  lineheight = 1.2))


# Area construida por Estado de conservacion
LaBarra_Estado_area <- aggregate(Area.construida ~ Estado.conservación , data = LaBarra, FUN = sum)
LaBarra_Estado_area$Porcentaje_AreaTotal <- LaBarra_Estado_area$Area.construida*100
LaBarra_Estado_area$Porcentaje_AreaTotal <- LaBarra_Estado_area$Area.construida/sum(LaBarra_Estado_area$Area.construida)
LaBarra_Estado_area$Porcentaje_AreaTotal <- percent(LaBarra_Estado_area$Porcentaje_AreaTotal)

# Ordenar
LaBarra_Estado_area$Estado.conservación <- factor(LaBarra_Estado_area$Estado.conservación,
                                                   levels = c("Excelente","Excelente/Bueno","Bueno","Bueno/Regular","Regular","Regular/Malo","Malo","Malo/Muy Malo" ,"Muy Malo","NA"))
LaBarra_Estado_area <- LaBarra_Estado_area[order(LaBarra_Estado_area$Estado.conservación),]

# Grafico
graf_Estado <- ggplot(LaBarra_Estado_area, aes(x = Estado.conservación, y = Area.construida)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Porcentaje_AreaTotal), vjust = -0.5,  size = 2) +
  xlab("Estado de conservación") + 
  ylab(bquote("Área construida"~(m^2))) +
  labs(caption = "Área total: 322.667" ~ m^2)+
  scale_x_discrete(labels = c("Excelente","Excelente/Bueno","Bueno",
                              "Bueno/Regular","Regular","Regular/Malo","Malo",
                              "Malo/Muy Malo" ,"Muy Malo","NA")) +
  scale_y_continuous(breaks = seq(from = 0, to = 128000, by = 25000))+
  theme(axis.text = element_text(size=8),
        plot.title = element_text(color = "black",
                                  hjust = 0.5, 
                                  size = 14, 
                                  lineheight = 1.2))

# Area construida por Régimen
LaBarra_Reg_area <- aggregate(Area.construida ~ Código.régimen , data = LaBarra, FUN = sum)
LaBarra_Reg_area$Porcentaje_AreaTotal <- LaBarra_Reg_area$Area.construida*100
LaBarra_Reg_area$Porcentaje_AreaTotal <- LaBarra_Reg_area$Area.construida/sum(LaBarra_Reg_area$Area.construida)
LaBarra_Reg_area$Porcentaje_AreaTotal <- percent(LaBarra_Reg_area$Porcentaje_AreaTotal)

# Gráfico
graf_Reg <- ggplot(LaBarra_Reg_area, aes(x = Código.régimen, y = Area.construida)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Porcentaje_AreaTotal), vjust = -0.5,  size = 2) + 
  xlab("Régimen") + 
  ylab(bquote("Área construida"~(m^2))) +
  labs(caption = "Área total: 339.868" ~ m^2)+
  scale_x_discrete(labels = c("PROP.COMÚN","PROP.HORIZONTAL","URBANIZACIÓN PH"))+
  theme(axis.text = element_text(size=8),
        plot.title = element_text(color = "black",
                                  hjust = 0.5, 
                                  size = 14, 
                                  lineheight = 1.2))



# Combinando gráficos

fil1 <- (graf_Estado | graf_CatCon) + plot_layout(heights = c(1,1))
fil2 <- (graf_Reg | graf_Dest) + plot_layout(widths = c(1,1))

graf_comb <- (fil1 / fil2) + plot_annotation(title = 'La Barra') & 
  theme(axis.text.x = element_text(size = 5.5),
        axis.text.y=element_text(angle = 90, vjust = 1, hjust = 0.5, size = 6.5),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title = element_text(color = "black",
                                  hjust = 0.5, 
                                  size = 14, 
                                  lineheight = 1.2))
graf_comb
ggsave(filename = "LaBarra_Area.png", plot = graf_comb, device = "png", 
       path = "salidas/Figuras/Básicas", width = 250, height = 150, units = "mm", 
       dpi = 500, limitsize = TRUE)

#Exportando individualmente
ggsave(filename = "Regimen_LB_Area.png", plot = graf_Reg, device = "png", 
       path = "salidas/Figuras/Básicas", width = 250, height = 150, units = "mm", 
       dpi = 500, limitsize = TRUE)

ggsave(filename = "Categoria_LB_Area.png", plot = graf_CatCon, device = "png", 
       path = "salidas/Figuras/Básicas", width = 250, height = 150, units = "mm", 
       dpi = 500, limitsize = TRUE)

ggsave(filename = "Destinos_LB_Area.png", plot = graf_Dest, device = "png", 
       path = "salidas/Figuras/Básicas", width = 300, height = 150, units = "mm", 
       dpi = 500, limitsize = TRUE)
ggsave(filename = "Estado_LB_Area.png", plot = graf_Estado, device = "png", 
       path = "salidas/Figuras/Básicas", width = 300, height = 150, units = "mm", 
       dpi = 500, limitsize = TRUE)

