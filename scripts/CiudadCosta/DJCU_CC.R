# PadronesUrbanos <- read.table("datos crudos/Padrones Urbanos.csv",
#                               header = FALSE, sep = ",")
# 
# PadronesUrb_CC <- PadronesUrbanos[PadronesUrbanos$V2 == "A" & PadronesUrbanos$V3 == "WA",]
# delete.na <- function(df, n=0) {
#   df[rowSums(is.na(df)) <= n,]
# }
# 
# PadronesUrb_CC_mod<- delete.na(PadronesUrb_CC)
# 
# names(PadronesUrb_CC_mod) = c("Código Régimen","Código Departamento", "Código Localidad",
#                           "Padrón","Block/Manzana", "EP/SS","Unidad","Área predio",
#                           "Área edificada", "Valor catastral terreno", "Valor catastral mejoras",
#                           "Valor catastral total", "Valor para impuestos", "Fecha última DJCU",
#                           "Vigencia última DJCU")
# 
# write.table(PadronesUrb_CC_mod, file = "datos procesados/Padrones Urbanos_CiudadCosta.csv",
#             row.names = FALSE, col.names = TRUE, sep = ",")

library(dplyr)
PadUrb_CC <- read.table("datos procesados/Padrones Urbanos_CiudadCosta.csv",
                        header = TRUE, sep = ",")

PadUrb_CC$DeclaraciónJurada <- ifelse(PadUrb_CC$`Fecha.última.DJCU` == "/  /", "No", "Si")
table(PadUrb_CC$DeclaraciónJurada,PadUrb_CC$Código.Régimen)

# No    Si 
# 24853 25014
# Total de padrones: 49867

#       CO    PH    UH
# No 22660  2193     0
# Si 13723 10050  1241



PadUrb_LB_filt <- PadUrb_LB[,c(4,16)]

PadUrb_LB_mod <- inner_join(LaBarra, PadUrb_LB_filt, by = "Padrón")
nrow(PadUrb_LB_mod)
filter(DeclaraciónJurada = Si)
str(PadUrb_LB_filt)
str(LaBarra)
