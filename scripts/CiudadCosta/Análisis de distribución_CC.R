# Paquetes
library(dplyr) # Calcular frecuencia
library(ggplot2) # Gráficos 
library(scales) # función percent
library(patchwork) #combinar distintos gráficos
library(ggtext)

# Datos
CiudadCosta <- read.table("datos procesados/Líneas de construcción_Ciudad de la Costa2.csv",
                          header = TRUE, sep = ",")

################################################################################

# FRECUENCIA DE APARICIÓN

################################################################################

# RÉGIMEN
  CiudadCosta_count_reg <- count(CiudadCosta, Código.régimen, name = "Freq.CR")
  # View(CiudadCosta_count_reg)
  total_count_reg <- sum(CiudadCosta_count_reg$Freq.CR)
  CiudadCosta_count_reg$prop <- CiudadCosta_count_reg$Freq.CR / total_count_reg
  
  # Grafico
  graf_reg <- ggplot(CiudadCosta_count_reg, aes(x = Código.régimen, y = prop)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = percent(prop)), vjust = -0.5,  size = 3) +
    xlab("Régimen") + 
    ylab("Porcentaje") +
    scale_x_discrete(labels = c("PROP.COMÚN","PROP.HORIZONTAL","URBANIZACIÓN PH")) +
    labs(title = "Ciudad de la Costa", caption = "Porcentaje por líneas de construcción, no por padrón") +
    theme_bw() +
    theme(plot.title = element_text(color = "black",
                                    hjust = 0.5, 
                                    size = 14, 
                                    lineheight = 1.2))


# CATEGORIAS DE CONSTRUCCION
  # Calcular porcentaje
  CiudadCosta_count_CatCon <- count(CiudadCosta, Categoría.de.construcción, name = "Freq.CC")
  total_count_CatCon <- sum(CiudadCosta_count_CatCon$Freq.CC)
  CiudadCosta_count_CatCon$prop <- CiudadCosta_count_CatCon$Freq.CC / total_count_CatCon
  
  # Ordenar las categorías
  CiudadCosta_count_CatCon$Categoría.de.construcción <- factor(CiudadCosta_count_CatCon$Categoría.de.construcción,
                                                               levels = c("Muy confortable","1.5","Confortable","2.5","Comun","3.5","Economica","4.5","Muy economica"))
  CiudadCosta_count_CatCon <- CiudadCosta_count_CatCon[order(CiudadCosta_count_CatCon$Categoría.de.construcción),]
  
  
  #Gráfico
  graf_CatCon <- ggplot(CiudadCosta_count_CatCon, aes(x = Categoría.de.construcción, y = prop)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = percent(prop)), vjust = -0.5, size = 3) +
    xlab("Categorías de construcción") + 
    ylab("Porcentaje") +
    scale_x_discrete(labels = c("Muy confortable","1.5","Confortable","2.5","Comun","3.5","Economica","4.5","Muy economica")) + 
    labs(title = "Ciudad de la Costa", caption = "Porcentaje por líneas de construcción, no por padrón")+
    theme_bw() +
    theme(plot.title = element_text(color = "black",
                                    hjust = 0.5, 
                                    size = 14, 
                                    lineheight = 1.2))



# DESTINOS
  # Calcular porcentaje
  CiudadCosta_count_Dest <- count(CiudadCosta, Destinos, name = "Freq.D")
  # View(CiudadCosta_count__Dest)
  total_count_Dest <- sum(CiudadCosta_count_Dest$Freq.D)
  CiudadCosta_count_Dest$prop <- CiudadCosta_count_Dest$Freq.D / total_count_Dest
  CiudadCosta_countfilt_Dest <- CiudadCosta_count_Dest[CiudadCosta_count_Dest$Freq.D > 700,]
  
  etiquetas <- CiudadCosta_countfilt_Dest$Destinos
  etiquetas[8] <- "CUB-TECH-COB"
  etiquetas[17] <- "PLAYA ESTA"
  etiquetas
  
  graf_Dest <- ggplot(CiudadCosta_countfilt_Dest, aes(x = Destinos, y = prop)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = percent(prop)), vjust = -0.5,  size = 3) +
    xlab("Destinos") + 
    ylab("Porcentaje") +
    scale_x_discrete(labels = etiquetas) +
    labs(title = "Ciudad de la Costa", caption = "Destinos con una frecuencia mayor a 700\nPorcentaje por líneas de construcción, no por padrón") +
    theme_bw() +
    theme(axis.text = element_text(size=5),
          plot.title = element_text(color = "black",
                                    hjust = 0.5, 
                                    size = 14, 
                                    lineheight = 1.2))


# ESTADOS DE CONSERVACIÓN
  # Calcular porcentaje
  CiudadCosta_count_Estado <- count(CiudadCosta, Estado.conservación, name = "Freq.Es")
  # View(CiudadCosta_count_Estado)
  total_count_Estado <- sum(CiudadCosta_count_Estado$Freq.Es)
  CiudadCosta_count_Estado$prop <- CiudadCosta_count_Estado$Freq.Es / total_count_Estado
  
  # Ordenar las categorías
  CiudadCosta_count_Estado$Estado.conservación <- factor(CiudadCosta_count_Estado$Estado.conservación,
                                                         levels = c("Excelente","Excelente/Bueno","Bueno","Bueno/Regular","Regular","Regular/Malo","Malo","Malo/Muy Malo" ,"Muy Malo","NA"))
  CiudadCosta_count_Estado <- CiudadCosta_count_Estado[order(CiudadCosta_count_Estado$Estado.conservación),]
  
  # Gráfico
  graf_Estado <- ggplot(CiudadCosta_count_Estado, aes(x = Estado.conservación, y = prop)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = percent(prop)), vjust = -0.5,  size = 3) +
    xlab("Estados de conservación") + 
    ylab("Porcentaje") +
    scale_x_discrete(labels = c("Excelente","Excelente/Bueno","Bueno","Bueno/Regular","Regular","Regular/Malo","Malo","Malo/Muy Malo" ,"Muy Malo","NA")) +
    labs(title = "Ciudad de la Costa", caption = "Porcentaje por líneas de construcción, no por padrón") +
    theme_bw() +
    theme(axis.text = element_text(size=8),
          plot.title = element_text(color = "black",
                                    hjust = 0.5, 
                                    size = 14, 
                                    lineheight = 1.2))


# Exportando figuras

  # ggsave(filename = "Regimen_CC.png", plot = graf_reg, device = "png",
  #        path = "salidas/Figuras/Básicas", width = 250, height = 150, units = "mm",
  #        dpi = 500, limitsize = TRUE)
  # 
  # ggsave(filename = "Categoria_construccion_CC.png", plot = graf_CatCon, device = "png",
  #        path = "salidas/Figuras/Básicas", width = 250, height = 150, units = "mm",
  #        dpi = 500, limitsize = TRUE)
  # 
  # ggsave(filename = "Destinos_CC.png", plot = graf_Dest, device = "png",
  #        path = "salidas/Figuras/Básicas", width = 300, height = 150, units = "mm",
  #        dpi = 500, limitsize = TRUE)
  # 
  # ggsave(filename = "Estado_conservacion_CC.png", plot = graf_Estado, device = "png",
  #        path = "salidas/Figuras/Básicas", width = 250, height = 150, units = "mm",
  #        dpi = 500, limitsize = TRUE)


################################################################################

# PORCENTAJE EN FUNCIÓN DEL ÁREA TOTAL DE LA LOCALIDAD

################################################################################

# Destino
CiudadCosta_Dest_area <- aggregate(Area.construida ~ Destinos , data = CiudadCosta, FUN = sum)
CiudadCosta_Dest_area$Porcentaje_AreaTotal <- CiudadCosta_Dest_area$Area.construida*100
CiudadCosta_Dest_area$Porcentaje_AreaTotal <- CiudadCosta_Dest_area$Area.construida/sum(CiudadCosta_Dest_area$Area.construida)
CiudadCosta_Dest_area$Porcentaje_AreaTotal <- percent(CiudadCosta_Dest_area$Porcentaje_AreaTotal)
CiudadCosta_Dest_area_mod <- CiudadCosta_Dest_area[CiudadCosta_Dest_area$Porcentaje_AreaTotal > 1,]

# Gráfico
graf_Dest <- ggplot(CiudadCosta_Dest_area_mod, aes(x = Destinos, y = Area.construida)) +
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


# Categoria de construccion
CiudadCosta_CatCon_area <- aggregate(Area.construida ~ Categoría.de.construcción , data = CiudadCosta, FUN = sum)
CiudadCosta_CatCon_area$Porcentaje_AreaTotal <- CiudadCosta_CatCon_area$Area.construida*100
CiudadCosta_CatCon_area$Porcentaje_AreaTotal <- CiudadCosta_CatCon_area$Area.construida/sum(CiudadCosta_CatCon_area$Area.construida)
CiudadCosta_CatCon_area$Porcentaje_AreaTotal <- percent(CiudadCosta_CatCon_area$Porcentaje_AreaTotal)


# Ordenar
CiudadCosta_CatCon_area$Categoría.de.construcción <- factor(CiudadCosta_CatCon_area$Categoría.de.construcción,
                                                            levels = c("Muy confortable","1.5","Confortable","2.5","Comun","3.5","Economica","4.5","Muy economica"))
CiudadCosta_CatCon_area <- CiudadCosta_CatCon_area[order(CiudadCosta_CatCon_area$Categoría.de.construcción),]

# Gráfico
graf_CatCon <- ggplot(CiudadCosta_CatCon_area, aes(x = Categoría.de.construcción, y = Area.construida)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Porcentaje_AreaTotal), vjust = -0.5,  size = 2) +
  xlab("Categoría de construcción") + 
  ylab(bquote("Área construida"~(m^2))) +
  scale_x_discrete(labels = c("Muy confortable","1.5","Confortable","2.5","Comun","3.5","Economica","4.5","Muy economica"))+
  scale_y_continuous(breaks = seq(from = 0, to = 2200000, by = 500000))+
  theme(axis.text = element_text(size=8),
        plot.title = element_text(color = "black",
                                  hjust = 0.5, 
                                  size = 14, 
                                  lineheight = 1.2))


# Estado de conservacion
CiudadCosta_Estado_area <- aggregate(Area.construida ~ Estado.conservación , data = CiudadCosta, FUN = sum)
CiudadCosta_Estado_area$Porcentaje_AreaTotal <- CiudadCosta_Estado_area$Area.construida*100
CiudadCosta_Estado_area$Porcentaje_AreaTotal <- CiudadCosta_Estado_area$Area.construida/sum(CiudadCosta_Estado_area$Area.construida)
CiudadCosta_Estado_area$Porcentaje_AreaTotal <- percent(CiudadCosta_Estado_area$Porcentaje_AreaTotal)

# Ordenar
CiudadCosta_Estado_area$Estado.conservación <- factor(CiudadCosta_Estado_area$Estado.conservación,
                                                      levels = c("Excelente","Excelente/Bueno","Bueno","Bueno/Regular","Regular","Regular/Malo","Malo","Malo/Muy Malo" ,"Muy Malo","NA"))
CiudadCosta_Estado_area <- CiudadCosta_Estado_area[order(CiudadCosta_Estado_area$Estado.conservación),]

# Grafico
graf_Estado <- ggplot(CiudadCosta_Estado_area, aes(x = Estado.conservación, y = Area.construida)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Porcentaje_AreaTotal), vjust = -0.5,  size = 2) +
  xlab("Estado de conservación") + 
  ylab(bquote("Área construida"~(m^2))) +
  scale_x_discrete(labels = c("Excelente","Excelente/Bueno","Bueno",
                              "Bueno/Regular","Regular","Regular/Malo","Malo",
                              "Malo/Muy Malo" ,"Muy Malo","NA")) +
  scale_y_continuous(breaks = seq(from = 0, to = 2200000, by = 500000))+
  theme(axis.text = element_text(size=8),
        plot.title = element_text(color = "black",
                                  hjust = 0.5, 
                                  size = 14, 
                                  lineheight = 1.2))


# Régimen
CiudadCosta_Reg_area <- aggregate(Area.construida ~ Código.régimen , data = CiudadCosta, FUN = sum)
CiudadCosta_Reg_area$Porcentaje_AreaTotal <- CiudadCosta_Reg_area$Area.construida*100
CiudadCosta_Reg_area$Porcentaje_AreaTotal <- CiudadCosta_Reg_area$Area.construida/sum(CiudadCosta_Reg_area$Area.construida)
CiudadCosta_Reg_area$Porcentaje_AreaTotal <- percent(CiudadCosta_Reg_area$Porcentaje_AreaTotal)

# Gráfico
graf_Reg <- ggplot(CiudadCosta_Reg_area, aes(x = Código.régimen, y = Area.construida)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Porcentaje_AreaTotal), vjust = -0.5,  size = 2) + 
  xlab("Régimen") + 
  ylab(bquote("Área construida"~(m^2))) +
  labs(caption = "Área total: 6.587.876" ~ m^2)+
  scale_x_discrete(labels = c("PROP.COMÚN","PROP.HORIZONTAL","URBANIZACIÓN PH"))+
  scale_y_continuous(breaks = seq(from = 0, to = 5300000, by = 1500000))+
  theme(axis.text = element_text(size=8),
        plot.title = element_text(color = "black",
                                  hjust = 0.5, 
                                  size = 14, 
                                  lineheight = 1.2))

# Combinando los gráficos
fil1 <- (graf_Estado | graf_CatCon) + plot_layout(heights = c(1,1))
fil2 <- (graf_Reg | graf_Dest) + plot_layout(widths = c(1,1))

graf_comb <- (fil1 / fil2) + plot_annotation(title = 'Ciudad de la Costa') & 
  theme(axis.text.x = element_text(size = 5.5),
        axis.text.y=element_text(angle = 90, vjust = 1, hjust = 0.5, size = 6.5),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title = element_text(color = "black",
                                  hjust = 0.5, 
                                  size = 14, 
                                  lineheight = 1.2))

# Exportando gráfico
# ggsave(filename = "CiudadCosta_Area.png", plot = graf_comb, device = "png", 
#        path = "salidas/Figuras/Básicas", width = 250, height = 150, units = "mm", 
#        dpi = 500, limitsize = TRUE)
# 
# # Gráficos individuales
# ggsave(filename = "Regimen_CC_Area.png", plot = graf_Reg, device = "png", 
#        path = "salidas/Figuras/Básicas", width = 250, height = 150, units = "mm", 
#        dpi = 500, limitsize = TRUE)
# 
# ggsave(filename = "Categoria_CC_Area.png", plot = graf_CatCon, device = "png", 
#        path = "salidas/Figuras/Básicas", width = 250, height = 150, units = "mm", 
#        dpi = 500, limitsize = TRUE)
# 
# ggsave(filename = "Destinos_CC_Area.png", plot = graf_Dest, device = "png", 
#        path = "salidas/Figuras/Básicas", width = 300, height = 150, units = "mm", 
#        dpi = 500, limitsize = TRUE)
# 
# ggsave(filename = "Estado_CC_Area.png", plot = graf_Estado, device = "png", 
#        path = "salidas/Figuras/Básicas", width = 250, height = 150, units = "mm", 
#        dpi = 500, limitsize = TRUE)

