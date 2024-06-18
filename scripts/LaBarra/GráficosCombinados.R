LaBarra <- read.table("datos procesados/Líneas de construcción_La barra2.csv",
                      header = TRUE, sep = ",")
str(LaBarra)

# Area construida por categoria de construccion
LaBarra_CatCon_area <- aggregate(Area.construida ~ Categoría.de.construcción , data = LaBarra, FUN = sum)
View(LaBarra_CatCon_area)
sum(LaBarra_CatCon_area$Area.construida) # Suma: 339868
LaBarra_CatCon_area$Porcentaje_AreaTotal <- LaBarra_CatCon_area$Area.construida*100
LaBarra_CatCon_area$Porcentaje_AreaTotal <- LaBarra_CatCon_area$Area.construida/339868
LaBarra_CatCon_area$Porcentaje_AreaTotal <- percent(LaBarra_CatCon_area$Porcentaje_AreaTotal)

# Ordenar
LaBarra_CatCon_area$Categoría.de.construcción <- factor(LaBarra_CatCon_area$Categoría.de.construcción,
                                                         levels = c("Muy economica","4.5", "Economica","3.5", "Comun","2.5", "Confortable","1.5", "Muy confortable"))
LaBarra_CatCon_area <- LaBarra_CatCon_area[order(LaBarra_CatCon_area$Categoría.de.construcción),]


# Area construida por estado de conservacion
LaBarra_Estado_area <- aggregate(Area.construida ~ Estado.conservación , data = LaBarra, FUN = sum)
View(LaBarra_Estado_area)
sum(LaBarra_Estado_area$Area.construida) # Suma: 322667
LaBarra_Estado_area$Porcentaje_AreaTotal <- LaBarra_Estado_area$Area.construida*100
LaBarra_Estado_area$Porcentaje_AreaTotal <- LaBarra_Estado_area$Area.construida/322667
LaBarra_Estado_area$Porcentaje_AreaTotal <- percent(LaBarra_Estado_area$Porcentaje_AreaTotal)

# Ordenar
LaBarra_Estado_area$Estado.conservación <- factor(LaBarra_Estado_area$Estado.conservación,
                                                   levels = c("Excelente","Excelente/Bueno","Bueno","Bueno/Regular","Regular","Regular/Malo","Malo","Malo/Muy Malo" ,"Muy Malo","NA"))
LaBarra_Estado_area <- LaBarra_Estado_area[order(LaBarra_Estado_area$Estado.conservación),]



graf_CatCon <- ggplot(LaBarra_CatCon_area, aes(x = Categoría.de.construcción, y = Area.construida)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Porcentaje), vjust = -0.5,  size = 3) +
  xlab("Categoría de construcción") + 
  ylab(bquote("Área construida"~(m^2))) +
  scale_x_discrete(labels = c("Muy economica","4.5", "Economica","3.5", "Comun","2.5", "Confortable","1.5", "Muy confortable")) +
  labs(title = "La Barra") +
  theme_bw() +
  theme(axis.text = element_text(size=8),
                plot.title = element_text(color = "black",
                                          hjust = 0.5, 
                                          size = 14, 
                                          lineheight = 1.2))

graf_CatCon

graf_Estado <- ggplot(LaBarra_Estado_area, aes(x = Estado.conservación, y = Area.construida)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Porcentaje), vjust = -0.5,  size = 3) +
  xlab("Estado de conservación") + 
  ylab(bquote("Área construida"~(m^2))) +
  scale_x_discrete(labels = c("Excelente","Excelente/Bueno","Bueno",
                              "Bueno/Regular","Regular","Regular/Malo","Malo",
                              "Malo/Muy Malo" ,"Muy Malo","NA")) +
  theme_bw() +
  theme(axis.text = element_text(size=8),
        plot.title = element_text(color = "black",
                                  hjust = 0.5, 
                                  size = 14, 
                                  lineheight = 1.2))

graf_Estado
