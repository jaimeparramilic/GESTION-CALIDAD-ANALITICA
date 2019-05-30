library(DBI)
library(ggplot2)
library(fitdistrplus)
rm(list = ls(all.names = TRUE))
#par(mfrow=c(1,1))
BD <- read.csv("E:/PROYECTO/Documentos/PAYC/PROYECTO 10. REPLANIFICACIÓN PAYC/ANALISIS/PROYECCIONES/FFIE/SCRIPTS/SCRIPTS ANTERIORES/DATOS_GERINT_DEPU.csv", header = TRUE, sep = ",")

simtot <- sample(BD$PORCENTAJE_DURACION,size=100000,replace=TRUE)

write.csv(simtot, file = "E:/PROYECTO/Documentos/PAYC/PROYECTO 10. REPLANIFICACIÓN PAYC/ANALISIS/PROYECCIONES/FFIE/SCRIPTS/SCRIPTS ANTERIORES/RANDOM.csv")