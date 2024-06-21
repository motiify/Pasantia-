# Cargar datos
LaBarra <- read.table("datos procesados/Líneas de construcción_La barra2.csv",
                      header = TRUE, sep = ",", stringsAsFactors = TRUE)
str(LaBarra)
View(LaBarra)

# Creación de los gráficos.
library(dplyr) # Calcular frecuencia
library(ggplot2) # Gráficos 
library(scales)

# RÉGIMEN
LaBarra_count_reg <- count(LaBarra, Código.régimen, name = "Freq.CR")
View(LaBarra_count_reg)
total_count_reg <- sum(LaBarra_count_reg$Freq.CR)
LaBarra_count_reg$prop <- LaBarra_count_reg$Freq.CR / total_count_reg

# Gráfico
graf_reg <- ggplot(LaBarra_count_reg, aes(x = Código.régimen, y = prop)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = percent(prop)), vjust = -0.5,  size = 3) + #podria multiplicar por 100 prop y esto no haría falta
  xlab("Régimen") + 
  ylab("Porcentaje") +
  scale_x_discrete(labels = c("PROP.COMÚN","PROP.HORIZONTAL","URBANIZACIÓN PH")) +
  labs(title = "La Barra") +
  theme_bw() +
  theme(plot.title = element_text(color = "black",
                                  hjust = 0.5, 
                                  size = 14, 
                                  lineheight = 1.2))
graf_reg #Visualizar gráfico


# CATEGORIA DE CONSTRUCCION
# Calcular porcentaje
LaBarra_count_CatCon <- count(LaBarra, Categoría.de.construcción, name = "Freq.CC")
View(LaBarra_count_CatCon)
total_count_CatCon <- sum(LaBarra_count_CatCon$Freq.CC)
LaBarra_count_CatCon$prop <- LaBarra_count_CatCon$Freq.CC / total_count_CatCon

# Ordenar las categorías
LaBarra_count_CatCon$Categoría.de.construcción <- factor(LaBarra_count_CatCon$Categoría.de.construcción,
                                                         levels = c("Muy economica","4.5", "Economica","3.5", "Comun","2.5", "Confortable","1.5", "Muy confortable"))
LaBarra_count_CatCon <- LaBarra_count_CatCon[order(LaBarra_count_CatCon$Categoría.de.construcción),]


# Gráfico
graf_CatCon <- ggplot(LaBarra_count_CatCon, aes(x = Categoría.de.construcción, y = prop)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = percent(prop)), vjust = -0.5,  size = 3) +
  xlab("Categorías de construcción") + 
  ylab("Porcentaje") +
  scale_x_discrete(labels = c("Muy economica","4.5", "Economica","3.5", "Comun","2.5", "Confortable","1.5", "Muy confortable")) +
  labs(title = "La Barra") +
  theme_bw() +
  theme(plot.title = element_text(color = "black",
                                  hjust = 0.5, 
                                  size = 14, 
                                  lineheight = 1.2))

graf_CatCon #Visualizar gráfico


# DESTINOS
# Calcular porcentaje
LaBarra_count_Dest <- count(LaBarra, Destinos, name = "Freq.D")
View(LaBarra_count_Dest)
total_count_Dest <- sum(LaBarra_count_Dest$Freq.D)
LaBarra_count_Dest$prop <- LaBarra_count_Dest$Freq.D / total_count_Dest
LaBarra_countfil_Dest <- LaBarra_count_Dest[LaBarra_count_Dest$Freq.D > 100,]

etiquetas <- LaBarra_countfil_Dest$Destinos
etiquetas[7] <- "CUB-TECH-COB"
etiquetas
# Gráfico

graf_Dest <- ggplot(LaBarra_count_Dest, aes(x = Destinos, y = prop)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = percent(prop)), vjust = -0.5,  size = 3) +
  xlab("Destinos") + 
  ylab("Porcentaje") +
  scale_x_discrete(labels = etiquetas) +
  labs(title = "La Barra", caption = "Destinos con una frecuencia mayor a 100") +
  facet_wrap(~ treat)
  theme_bw() +
  theme(axis.text = element_text(size=6),
          plot.title = element_text(color = "black",
                                    hjust = 0.5, 
                                    size = 14, 
                                    lineheight = 1.2))
graf_Dest #Visualizar gráfico

# ESTADOS DE CONSERVACIÓN
# Calcular porcentaje
LaBarra_count_Estado <- count(LaBarra, Estado.conservación, name = "Freq.Es")
View(LaBarra_count_Estado)
total_count_Estado <- sum(LaBarra_count_Estado$Freq.Es)
LaBarra_count_Estado$prop <- LaBarra_count_Estado$Freq.Es / total_count_Estado

# Ordenar las categorías
LaBarra_count_Estado$Estado.conservación <- factor(LaBarra_count_Estado$Estado.conservación,
                                                   levels = c("Excelente","Excelente/Bueno","Bueno","Bueno/Regular","Regular","Regular/Malo","Malo","Malo/Muy Malo" ,"Muy Malo","NA"))
LaBarra_count_Estado <- LaBarra_count_Estado[order(LaBarra_count_Estado$Estado.conservación),]

# Gráfico
graf_Estado <- ggplot(LaBarra_count_Estado, aes(x = Estado.conservación, y = prop)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = percent(prop)), vjust = -0.5,  size = 3) +
  xlab("Estados de conservación") + 
  ylab("Porcentaje") +
  scale_x_discrete(labels = c("Excelente","Excelente/Bueno","Bueno","Bueno/Regular","Regular","Regular/Malo","Malo","Malo/Muy Malo" ,"Muy Malo","NA")) +
  labs(title = "La Barra") +
  theme_bw() +
  theme(axis.text = element_text(size=8),
        plot.title = element_text(color = "black",
                                  hjust = 0.5, 
                                  size = 14, 
                                  lineheight = 1.2))
graf_Estado #Visualizar gráfico


# Exportando figuras

ggsave(filename = "Regimen_LB.png", plot = graf_reg, device = "png", 
       path = "salidas/Figuras", width = 250, height = 150, units = "mm", 
       dpi = 500, limitsize = TRUE)

ggsave(filename = "Categoria_construccion_LB.png", plot = graf_CatCon, device = "png", 
       path = "salidas/Figuras", width = 250, height = 150, units = "mm", 
       dpi = 500, limitsize = TRUE)

ggsave(filename = "Destinos_LB.png", plot = graf_Dest, device = "png", 
       path = "salidas/Figuras", width = 300, height = 150, units = "mm", 
       dpi = 500, limitsize = TRUE)
ggsave(filename = "Estado_conservacion_LB.png", plot = graf_Estado, device = "png", 
       path = "salidas/Figuras", width = 300, height = 150, units = "mm", 
       dpi = 500, limitsize = TRUE)
