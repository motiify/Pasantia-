a <- count(LaBarra, Padrón, wt=NULL, name = "Cantidad")
View(a)
a$Unico <- a$Cantidad/a$Cantidad
summarise(a)
str(a)
summary(a)
a <- a[,c(1,2)]
names(a) <- c("Padrones Lineas", "Cantidad Padrones Lineas")
b <- count(PadUrb_LB, Padrón, wt=NULL, name = "Cantidad Padrones Parcelas")
View(b)
c <- count(PadLine_LB, Padrón, wt = NULL)