# PadronesUrbanos <- read.table("datos crudos/Padrones Urbanos.csv",
#                               header = FALSE, sep = ",")
# View(PadronesUrb_LB)
# PadronesUrb_LB <- PadronesUrbanos[PadronesUrbanos$V2 == "B" & PadronesUrbanos$V3 == "EH",]
# names(PadronesUrb_LB) = c("Código Régimen","Código Departamento", "Código Localidad",
#                           "Padrón","Block/Manzana", "EP/SS","Unidad","Área predio",
#                           "Área edificada", "Valor catastral terreno", "Valor catastral mejoras",
#                           "Valor catastral total", "Valor para impuestos", "Fecha última DJCU",
#                           "Vigencia última DJCU")
# 
# write.table(PadronesUrb_LB, file = "datos procesados/Padrones Urbanos_LaBarra.csv",
#             row.names = FALSE, col.names = TRUE, sep = ",")

library(dplyr)
PadUrb_LB <- read.table("datos procesados/Padrones Urbanos_LaBarra.csv",
                        header = TRUE, sep = ",")

PadUrb_LB$DeclaraciónJurada <- ifelse(PadUrb_LB$`Fecha.última.DJCU` == "/  /", "No", "Si")
PadUrb_LB_filt <- PadUrb_LB[,c(4,16)]

PadUrb_LB_mod <- inner_join(LaBarra, PadUrb_LB_filt, by = "Padrón")
nrow(PadUrb_LB_mod)
filter(DeclaraciónJurada = Si)
str(PadUrb_LB_filt)
str(LaBarra)
