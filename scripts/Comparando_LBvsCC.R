# Paquetes
library(dplyr) # Calcular frecuencia
library(ggplot2) # Gráficos 
library(scales)

# Cargar datos
LaBarra <- read.table("datos procesados/Líneas de construcción_La barra2.csv",
                      header = TRUE, sep = ",", stringsAsFactors = TRUE)

CiudadCosta <- read.table("datos procesados/Líneas de construcción_Ciudad de la Costa2.csv",
                      header = TRUE, sep = ",", stringsAsFactors = TRUE)



# RÉGIMEN
LaBarra_count_reg <- count(LaBarra, Código.régimen, name = "Freq.CR")
total_count_reg <- sum(LaBarra_count_reg$Freq.CR)
LaBarra_count_reg$prop <- LaBarra_count_reg$Freq.CR / total_count_reg
LaBarra_count_reg$percent <- percent(LaBarra_count_reg$prop)
LaBarra_count_reg$Localidad <- "La Barra"

CiudadCosta_count_reg <- count(CiudadCosta, Código.régimen, name = "Freq.CR")
total_count_reg <- sum(CiudadCosta_count_reg$Freq.CR)
CiudadCosta_count_reg$prop <- CiudadCosta_count_reg$Freq.CR / total_count_reg
CiudadCosta_count_reg$percent <- percent(CiudadCosta_count_reg$prop)
CiudadCosta_count_reg$Localidad <- "Ciudad de la Costa"

Reg_comb <- rbind(LaBarra_count_reg,CiudadCosta_count_reg)

Reg <- ggplot(Reg_comb, aes(x = Código.régimen, y = prop, fill = Localidad)) + 
  geom_bar(stat = "identity")+    #crear un gráfico de barras apiladas para múltiples variables
  ylab("Proporción")+
  xlab("Régimen")


# CATEGORIA DE CONSTRUCCION
LaBarra_count_CatCon <- count(LaBarra, Categoría.de.construcción, name = "Freq.CC")
total_count_CatCon <- sum(LaBarra_count_CatCon$Freq.CC)
LaBarra_count_CatCon$prop <- LaBarra_count_CatCon$Freq.CC / total_count_CatCon
LaBarra_count_CatCon$percent <- percent(LaBarra_count_CatCon$prop)
LaBarra_count_CatCon$Localidad <- "La Barra"
LaBarra_count_CatCon$Categoría.de.construcción <- factor(LaBarra_count_CatCon$Categoría.de.construcción,
                                                         levels = c("Muy economica","4.5", "Economica","3.5", "Comun","2.5", "Confortable","1.5", "Muy confortable"))
LaBarra_count_CatCon <- LaBarra_count_CatCon[order(LaBarra_count_CatCon$Categoría.de.construcción),]


CiudadCosta_count_CatCon <- count(CiudadCosta, Categoría.de.construcción, name = "Freq.CC")
total_count_CatCon <- sum(CiudadCosta_count_CatCon$Freq.CC)
CiudadCosta_count_CatCon$prop <- CiudadCosta_count_CatCon$Freq.CC / total_count_CatCon
CiudadCosta_count_CatCon$percent <- percent(CiudadCosta_count_CatCon$prop)
CiudadCosta_count_CatCon$Localidad <- "Ciudad de la Costa"
CiudadCosta_count_CatCon$Categoría.de.construcción <- factor(CiudadCosta_count_CatCon$Categoría.de.construcción,
                                                         levels = c("Muy economica","4.5", "Economica","3.5", "Comun","2.5", "Confortable","1.5", "Muy confortable"))
CiudadCosta_count_CatCon <- CiudadCosta_count_CatCon[order(CiudadCosta_count_CatCon$Categoría.de.construcción),]

CatCon_comb <- rbind(LaBarra_count_CatCon,CiudadCosta_count_CatCon)

CatCon <- ggplot(CatCon_comb, aes(x = Categoría.de.construcción, y = prop, fill = Localidad)) + 
  geom_bar(stat = "identity")+    #crear un gráfico de barras apiladas para múltiples variables
  ylab("Proporción")+
  xlab("Categorías de construcción")

# ESTADOS DE CONSERVACIÓN
LaBarra_count_Estado <- count(LaBarra, Estado.conservación, name = "Freq.Es")
total_count_Estado <- sum(LaBarra_count_Estado$Freq.Es)
LaBarra_count_Estado$prop <- LaBarra_count_Estado$Freq.Es / total_count_Estado
LaBarra_count_Estado$percent <- percent(LaBarra_count_Estado$prop)
LaBarra_count_Estado$Localidad <- "La Barra"
LaBarra_count_Estado$Estado.conservación <- factor(LaBarra_count_Estado$Estado.conservación,
                                                   levels = c("Excelente","Excelente/Bueno","Bueno","Bueno/Regular","Regular","Regular/Malo","Malo","Malo/Muy Malo" ,"Muy Malo","NA"))
LaBarra_count_Estado <- LaBarra_count_Estado[order(LaBarra_count_Estado$Estado.conservación),]

CiudadCosta_count_Estado <- count(CiudadCosta, Estado.conservación, name = "Freq.Es")
total_count_Estado <- sum(CiudadCosta_count_Estado$Freq.Es)
CiudadCosta_count_Estado$prop <- CiudadCosta_count_Estado$Freq.Es / total_count_Estado
CiudadCosta_count_Estado$percent <- percent(CiudadCosta_count_Estado$prop)
CiudadCosta_count_Estado$Localidad <- "Ciudad de la Costa"
CiudadCosta_count_Estado$Estado.conservación <- factor(CiudadCosta_count_Estado$Estado.conservación,
                                                   levels = c("Excelente","Excelente/Bueno","Bueno","Bueno/Regular","Regular","Regular/Malo","Malo","Malo/Muy Malo" ,"Muy Malo","NA"))
CiudadCosta_count_Estado <- CiudadCosta_count_Estado[order(CiudadCosta_count_Estado$Estado.conservación),]

Estados_comb <- rbind(LaBarra_count_Estado,CiudadCosta_count_Estado)
Estado <- ggplot(Estados_comb, aes(x = Estado.conservación, y = prop, fill = Localidad)) + 
  geom_bar(stat = "identity")+    #crear un gráfico de barras apiladas para múltiples variables
  ylab("Proporción") + 
  xlab("Estados de conservación")



# fil1 <- (Estado | CatCon) + plot_layout(heights = c(1,1))
# fil2 <- (plot_spacer() | Reg | plot_spacer()) + plot_layout(widths = c(0.25,0.5,0.25))
# fil2 <- Reg

graf_comb <- Estado + CatCon + Reg +  guide_area() + plot_layout(guides = 'collect',axis_titles = "collect")

Combinado <- graf_comb + plot_annotation(title = 'La Barra vs Ciudad de la Costa') &
  theme(axis.text.x = element_text(size = 5.5),
        axis.text.y=element_text(angle = 90, vjust = 1, hjust = 0.5, size = 6.5),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title = element_text(color = "black",
                                  hjust = 0.5,
                                  size = 14,
                                  lineheight = 1.2))



# Exportando figuras
ggsave(filename = "Comparando_CategoríaConstrucción.png", plot = CatCon, device = "png", 
       path = "salidas/Figuras/Combinadas", width = 250, height = 150, units = "mm", 
       dpi = 500, limitsize = TRUE)
ggsave(filename = "Comparando_Régimen.png", plot = Reg, device = "png", 
       path = "salidas/Figuras/Combinadas", width = 250, height = 150, units = "mm", 
       dpi = 500, limitsize = TRUE)
ggsave(filename = "Comparando_EstadoConservación.png", plot = Estado, device = "png", 
       path = "salidas/Figuras/Combinadas", width = 250, height = 150, units = "mm", 
       dpi = 500, limitsize = TRUE)
ggsave(filename = "Combinando todo.png", plot = Combinado, device = "png", 
       path = "salidas/Figuras/Combinadas", width = 250, height = 150, units = "mm", 
       dpi = 500, limitsize = TRUE)
