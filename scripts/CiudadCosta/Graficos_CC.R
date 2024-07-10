# Cargar datos
CiudadCosta <- read.table("datos procesados/Líneas de construcción_Ciudad de la Costa2.csv",
                      header = TRUE, sep = ",")
# str(CiudadCosta)
# View(CiudadCosta)


# Creación de los gráficos.
library(dplyr) # Calcular frecuencia
library(ggplot2) # Gráficos 
library(scales) # función percent


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
graf_reg #Visualizar gráfico


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

graf_CatCon #Visualizar gráfico


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
graf_Dest #Visualizar gráfico


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
graf_Estado #Visualizar gráfico


# Exportando figuras

ggsave(filename = "Regimen_CC.png", plot = graf_reg, device = "png", 
       path = "salidas/Figuras/Básicas", width = 250, height = 150, units = "mm", 
       dpi = 500, limitsize = TRUE)

ggsave(filename = "Categoria_construccion_CC.png", plot = graf_CatCon, device = "png", 
       path = "salidas/Figuras/Básicas", width = 250, height = 150, units = "mm", 
       dpi = 500, limitsize = TRUE)

ggsave(filename = "Destinos_CC.png", plot = graf_Dest, device = "png", 
       path = "salidas/Figuras/Básicas", width = 300, height = 150, units = "mm", 
       dpi = 500, limitsize = TRUE)

ggsave(filename = "Estado_conservacion_CC.png", plot = graf_Estado, device = "png", 
       path = "salidas/Figuras/Básicas", width = 250, height = 150, units = "mm", 
       dpi = 500, limitsize = TRUE)
