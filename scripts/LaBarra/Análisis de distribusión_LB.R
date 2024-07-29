# Paquetes
library(dplyr) # Calcular frecuencia
library(ggplot2) # Gráficos 
library(scales) #función percent.
library(patchwork) #combinar distintos gráficos

# Datos
LaBarra <- read.table("datos procesados/Líneas de construcción_La barra2.csv",
                      header = TRUE, sep = ",")

################################################################################

# FRECUENCIA DE APARICIÓN

################################################################################

# RÉGIMEN
  LaBarra_count_reg <- count(LaBarra, Código.régimen, name = "Freq.CR")
  total_count_reg <- sum(LaBarra_count_reg$Freq.CR)
  LaBarra_count_reg$prop <- LaBarra_count_reg$Freq.CR / total_count_reg
  
  # Gráfico
  Freq_Reg <- ggplot(LaBarra_count_reg, aes(x = Código.régimen, y = prop)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = percent(prop)), vjust = -0.5,  size = 3) + #podria multiplicar por 100 prop y esto no haría falta
    xlab("Régimen") + 
    ylab("Porcentaje") +
    scale_x_discrete(labels = c("PROP.COMÚN","PROP.HORIZONTAL","URBANIZACIÓN PH")) +
    labs(title = "La Barra", caption = "Porcentaje por líneas de construcción, no por padrón") +
    theme_bw() +
    theme(plot.title = element_text(color = "black",
                                    hjust = 0.5, 
                                    size = 14, 
                                    lineheight = 1.2))

# CATEGORIA DE CONSTRUCCION
  # Calcular porcentaje
  LaBarra_count_CatCon <- count(LaBarra, Categoría.de.construcción, name = "Freq.CC")
  # View(LaBarra_count_CatCon)
  total_count_CatCon <- sum(LaBarra_count_CatCon$Freq.CC)
  LaBarra_count_CatCon$prop <- LaBarra_count_CatCon$Freq.CC / total_count_CatCon
  
  # Ordenar las categorías
  LaBarra_count_CatCon$Categoría.de.construcción <- factor(LaBarra_count_CatCon$Categoría.de.construcción,
                                                           levels = c("Muy confortable","1.5","Confortable","2.5","Comun","3.5","Economica","4.5","Muy economica"))
  LaBarra_count_CatCon <- LaBarra_count_CatCon[order(LaBarra_count_CatCon$Categoría.de.construcción),]
  
  
  # Gráfico
  Freq_CatCon <- ggplot(LaBarra_count_CatCon, aes(x = Categoría.de.construcción, y = prop)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = percent(prop)), vjust = -0.5,  size = 3) +
    xlab("Categorías de construcción") + 
    ylab("Porcentaje") +
    scale_x_discrete(labels = c("Muy confortable","1.5","Confortable","2.5","Comun","3.5","Economica","4.5","Muy economica")) +
    labs(title = "La Barra", caption = "Porcentaje por líneas de construcción, no por padrón") +
    theme_bw() +
    theme(plot.title = element_text(color = "black",
                                    hjust = 0.5, 
                                    size = 14, 
                                    lineheight = 1.2))
  
# DESTINOS
  # Calcular porcentaje
  LaBarra_count_Dest <- count(LaBarra, Destinos, name = "Freq.D")
  # View(LaBarra_count_Dest)
  total_count_Dest <- sum(LaBarra_count_Dest$Freq.D)
  LaBarra_count_Dest$prop <- LaBarra_count_Dest$Freq.D / total_count_Dest
  LaBarra_countfilt_Dest <- LaBarra_count_Dest[LaBarra_count_Dest$Freq.D > 100,]
  LaBarra_countfilt_Dest$Destinos[7] <- "CUB-TECH-COB"
  
  
  # etiquetas <- LaBarra_countfilt_Dest$Destinos
  # etiquetas[7] <- "CUB-TECH-COB"
  # etiquetas[17] <- "PLAYA ESTA"
  # etiquetas
  
  Freq_Dest <- ggplot(LaBarra_countfilt_Dest, aes(x = Destinos, y = prop)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = percent(prop)), vjust = -0.5,  size = 3) +
    xlab("Destinos") + 
    ylab("Porcentaje") +
    scale_x_discrete(labels = LaBarra_countfilt_Dest$Destinos) +
    labs(title = "La Barra", caption = "Destinos con una frecuencia mayor a 700\nPorcentaje por líneas de construcción, no por padrón") +
    theme_bw() +
    theme(axis.text = element_text(size=5),
          plot.title = element_text(color = "black",
                                    hjust = 0.5, 
                                    size = 14, 
                                    lineheight = 1.2))

# ESTADOS DE CONSERVACIÓN
  # Calcular porcentaje
  LaBarra_count_Estado <- count(LaBarra, Estado.conservación, name = "Freq.Es")
  # View(LaBarra_count_Estado)
  total_count_Estado <- sum(LaBarra_count_Estado$Freq.Es)
  LaBarra_count_Estado$prop <- LaBarra_count_Estado$Freq.Es / total_count_Estado
  
  # Ordenar las categorías
  LaBarra_count_Estado$Estado.conservación <- factor(LaBarra_count_Estado$Estado.conservación,
                                                     levels = c("Excelente","Excelente/Bueno","Bueno","Bueno/Regular","Regular","Regular/Malo","Malo","Malo/Muy Malo" ,"Muy Malo","NA"))
  LaBarra_count_Estado <- LaBarra_count_Estado[order(LaBarra_count_Estado$Estado.conservación),]
  
  # Gráfico
  Freq_Estado <- ggplot(LaBarra_count_Estado, aes(x = Estado.conservación, y = prop)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = percent(prop)), vjust = -0.5,  size = 3) +
    xlab("Estados de conservación") + 
    ylab("Porcentaje") +
    scale_x_discrete(labels = c("Excelente","Excelente/Bueno","Bueno","Bueno/Regular","Regular","Regular/Malo","Malo","Malo/Muy Malo" ,"Muy Malo","NA")) +
    labs(title = "La Barra", caption = "Porcentaje por líneas de construcción, no por padrón") +
    theme_bw() +
    theme(axis.text = element_text(size=8),
          plot.title = element_text(color = "black",
                                    hjust = 0.5, 
                                    size = 14, 
                                    lineheight = 1.2))



# Exportando figuras

  # ggsave(filename = "Regimen_LB.png", plot = Freq_Reg, device = "png", 
  #        path = "salidas/Figuras/Básicas", width = 250, height = 150, units = "mm", 
  #        dpi = 500, limitsize = TRUE)
  # 
  # ggsave(filename = "Categoria_construccion_LB.png", plot = Freq_CatCon, device = "png", 
  #        path = "salidas/Figuras/Básicas", width = 250, height = 150, units = "mm", 
  #        dpi = 500, limitsize = TRUE)
  # 
  # ggsave(filename = "Destinos_LB.png", plot = Freq_Dest, device = "png", 
  #        path = "salidas/Figuras/Básicas", width = 300, height = 150, units = "mm", 
  #        dpi = 500, limitsize = TRUE)
  # ggsave(filename = "Estado_conservacion_LB.png", plot = Freq_Estado, device = "png", 
  #        path = "salidas/Figuras/Básicas", width = 300, height = 150, units = "mm", 
  #        dpi = 500, limitsize = TRUE)

################################################################################
  
# PORCENTAJE EN FUNCIÓN DEL ÁREA TOTAL DE LA LOCALIDAD
  
################################################################################

# RÉGIMEN
  LaBarra_Reg_area <- aggregate(Area.construida ~ Código.régimen , data = LaBarra, FUN = sum)
  LaBarra_Reg_area$Porcentaje_AreaTotal <- LaBarra_Reg_area$Area.construida*100
  LaBarra_Reg_area$Porcentaje_AreaTotal <- LaBarra_Reg_area$Area.construida/sum(LaBarra_Reg_area$Area.construida)
  LaBarra_Reg_area$Porcentaje_AreaTotal <- percent(LaBarra_Reg_area$Porcentaje_AreaTotal)
  
  # Gráfico
  Area_Reg <- ggplot(LaBarra_Reg_area, aes(x = Código.régimen, y = Area.construida)) +
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

# CATEGORÍA DE CONSTRUCCIÓN
  LaBarra_CatCon_area <- aggregate(Area.construida ~ Categoría.de.construcción , data = LaBarra, FUN = sum)
  LaBarra_CatCon_area$Porcentaje_AreaTotal <- LaBarra_CatCon_area$Area.construida*100
  LaBarra_CatCon_area$Porcentaje_AreaTotal <- LaBarra_CatCon_area$Area.construida/sum(LaBarra_CatCon_area$Area.construida)
  LaBarra_CatCon_area$Porcentaje_AreaTotal <- percent(LaBarra_CatCon_area$Porcentaje_AreaTotal)
  
  
  # Ordenar
  LaBarra_CatCon_area$Categoría.de.construcción <- factor(LaBarra_CatCon_area$Categoría.de.construcción,
                                                          levels = c("Muy confortable","1.5","Confortable","2.5","Comun","3.5","Economica","4.5","Muy economica"))
  LaBarra_CatCon_area <- LaBarra_CatCon_area[order(LaBarra_CatCon_area$Categoría.de.construcción),]
  
  # Gráfico
  Area_CatCon <- ggplot(LaBarra_CatCon_area, aes(x = Categoría.de.construcción, y = Area.construida)) +
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
  
# DESTINO
  LaBarra_Dest_area <- aggregate(Area.construida ~ Destinos , data = LaBarra, FUN = sum)
  LaBarra_Dest_area$Porcentaje_AreaTotal <- LaBarra_Dest_area$Area.construida*100
  LaBarra_Dest_area$Porcentaje_AreaTotal <- LaBarra_Dest_area$Area.construida/sum(LaBarra_Dest_area$Area.construida)
  LaBarra_Dest_area$Porcentaje_AreaTotal <- percent(LaBarra_Dest_area$Porcentaje_AreaTotal)
  LaBarra_Dest_area_mod <- LaBarra_Dest_area[LaBarra_Dest_area$Porcentaje_AreaTotal > 1,]
  
  # Gráfico
  Area_Dest <- ggplot(LaBarra_Dest_area_mod, aes(x = Destinos, y = Area.construida)) +
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


# ESTADO DE CONSERVACIÓN
  LaBarra_Estado_area <- aggregate(Area.construida ~ Estado.conservación , data = LaBarra, FUN = sum)
  LaBarra_Estado_area$Porcentaje_AreaTotal <- LaBarra_Estado_area$Area.construida*100
  LaBarra_Estado_area$Porcentaje_AreaTotal <- LaBarra_Estado_area$Area.construida/sum(LaBarra_Estado_area$Area.construida)
  LaBarra_Estado_area$Porcentaje_AreaTotal <- percent(LaBarra_Estado_area$Porcentaje_AreaTotal)
  
  # Ordenar
  LaBarra_Estado_area$Estado.conservación <- factor(LaBarra_Estado_area$Estado.conservación,
                                                    levels = c("Excelente","Excelente/Bueno","Bueno","Bueno/Regular","Regular","Regular/Malo","Malo","Malo/Muy Malo" ,"Muy Malo","NA"))
  LaBarra_Estado_area <- LaBarra_Estado_area[order(LaBarra_Estado_area$Estado.conservación),]
  
  # Grafico
  Area_Estado <- ggplot(LaBarra_Estado_area, aes(x = Estado.conservación, y = Area.construida)) +
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




# Combinando los gráficos

  fil1 <- (Area_Estado | Area_CatCon) + plot_layout(heights = c(1,1))
  fil2 <- (Area_Reg | Area_Dest) + plot_layout(widths = c(1,1))
  
  Area_comb <- (fil1 / fil2) + plot_annotation(title = 'La Barra') & 
    theme(axis.text.x = element_text(size = 5.5),
          axis.text.y=element_text(angle = 90, vjust = 1, hjust = 0.5, size = 6.5),
          axis.title.x = element_text(size = 8),
          axis.title.y = element_text(size = 8),
          plot.title = element_text(color = "black",
                                    hjust = 0.5, 
                                    size = 14, 
                                    lineheight = 1.2))

# Exportando gráfico
  # ggsave(filename = "LaBarra_Area.png", plot = Area_comb, device = "png", 
  #        path = "salidas/Figuras/Básicas", width = 250, height = 150, units = "mm", 
  #        dpi = 500, limitsize = TRUE)
  # 
  # # Gráficos individuales
  #
  # ggsave(filename = "Regimen_LB_Area.png", plot = Area_Reg, device = "png", 
  #        path = "salidas/Figuras/Básicas", width = 250, height = 150, units = "mm", 
  #        dpi = 500, limitsize = TRUE)
  # 
  # ggsave(filename = "Categoria_LB_Area.png", plot = Area_CatCon, device = "png", 
  #        path = "salidas/Figuras/Básicas", width = 250, height = 150, units = "mm", 
  #        dpi = 500, limitsize = TRUE)
  # 
  # ggsave(filename = "Destinos_LB_Area.png", plot = Area_Dest, device = "png", 
  #        path = "salidas/Figuras/Básicas", width = 300, height = 150, units = "mm", 
  #        dpi = 500, limitsize = TRUE)
  # ggsave(filename = "Estado_LB_Area.png", plot = Area_Estado, device = "png", 
  #        path = "salidas/Figuras/Básicas", width = 300, height = 150, units = "mm", 
  #        dpi = 500, limitsize = TRUE)