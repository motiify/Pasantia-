# Cargar datos
LaBarra <- read.table("datos procesados/Líneas de construcción_La barra.csv",
                      sep = ",", header = TRUE)
str(LaBarra)
View(LaBarra)
LaBarra$AreaTot <-
SumaTot <- function(df){for (i in 1:nrows(LaBarra)) {
  if (df$Padrón[i,4] == df$Padrón[i+1,4] ) {suma <- suma + A[i,3]}
}
LaBarra[,4]

LaBarra$Padrón <- apply(LaBarra, MARGIN=1, FUN=function(x) {sum(x[which(x==89)+1], na.rm = TRUE)})
https://es.stackoverflow.com/questions/221073/c%C3%B3mo-sumar-columnas-en-funci%C3%B3n-de-otras-columnas