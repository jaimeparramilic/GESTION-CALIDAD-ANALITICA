library(DBI)
library(ggplot2)
library(fitdistrplus)
library(ggpubr)
library(rpart)
library(tree)
library(randomForest)
library(rpart.plot)
library(fitdistrplus)
library(stringr)
library(dplyr)
library(triangle)
library(mc2d)
library(lattice)
library(PerformanceAnalytics)
library(sfsmisc)
library(HistogramTools)
library(fPortfolio)
library(party)
library(relaimpo)
library(caret)
library(earth)
library(Boruta)
#LIMPIAR VARIABLES
rm(list = ls(all.names = TRUE))

#Cargue de datos
BD <- read.csv("E:/PROYECTO/Documentos/PAYC/PROYECTO 10. REPLANIFICACIÓN PAYC/ANALISIS/OTRO SÍS, LA REC. DE CLI. Y LA SAT.  DE LOS CLI/SCRIPTS/BASE DE DATOS PARA CORRIDA.csv", header = TRUE, sep = ",")

#Parición de los datos
set.seed(3457)
indice <- createDataPartition(BD$MULTIPLICADOR, p = .67,
                                  list = FALSE,
                                  times = 1)
BD_ENTRENO <-BD[indice,]
BD_CALIBRACION <-BD[-indice,]


#VARIABLE RETORNADO
#1. MÉTODO RANDOM FOREST
set.seed(3457)
m_rf_r <-randomForest(BD$MULTIPLICADOR ~ BD$COMPROMISOS + BD$SATISFACCION+BD$PERSONAL+BD$VOLVER.A.CONTRATAR+BD$PRODUCTOS+BD$SOPORTE_ADMON+BD$SOPORTE_DIR+BD$SOPORTE_TEC, importance=TRUE,ntree=100000)
error_rf_r<-mean(sqrt(m_rf_r[["mse"]]))


#m_rf_r$importance[,3]

#2. IMPORTANCIA RELATIVA -> REGRESIÓN LINEAL
m_rl_r_e<-lm(MULTIPLICADOR   ~ COMPROMISOS + SATISFACCION+PERSONAL+VOLVER.A.CONTRATAR+PRODUCTOS+SOPORTE_ADMON+SOPORTE_DIR+SOPORTE_TEC,data=BD_ENTRENO)
m_rl_r<-lm(BD$MULTIPLICADOR  ~ BD$COMPROMISOS + BD$SATISFACCION+BD$PERSONAL+BD$VOLVER.A.CONTRATAR+BD$PRODUCTOS+BD$SOPORTE_ADMON+BD$SOPORTE_DIR+BD$SOPORTE_TEC)
imp_rl_r <-calc.relimp(m_rl_r, type = "lmg", rela = TRUE) 
summary(m_rl_r)
BD_CALIBRACION$pred_rl_r <-predict(m_rl_r_e, select(BD_CALIBRACION,COMPROMISOS , SATISFACCION,PERSONAL,VOLVER.A.CONTRATAR,PRODUCTOS,SOPORTE_ADMON,SOPORTE_DIR,SOPORTE_TEC), type = "response")
#BD_CALIBRACION <- BD_CALIBRACION  %>% mutate(pred_rl_r1 = 1*(pred_rl_r > .53) + 0)
#BD_CALIBRACION <- BD_CALIBRACION  %>% mutate(accurate = 1*(pred_rl_r1 == RETORNADO))
error_rl_r <- 1 - (sqrt(sum((BD_CALIBRACION$pred_rl_r - BD_CALIBRACION$MULTIPLICADOR)*(BD_CALIBRACION$pred_rl_r - BD_CALIBRACION$MULTIPLICADOR)))/nrow(BD_CALIBRACION))


#3. IMPORTANCIA RELATIVA -> MARS -> NO APLICA
#4. REGRESIÓN LINEAL -> NO APLICA
#5. BORUTA
i=1
#warnings()
bortotr <- data.frame(a="",b="",c=0, stringsAsFactors=FALSE)
while(i<=10){
set.seed(as.numeric(Sys.time()))
m_bor_r <-Boruta(BD$MULTIPLICADOR  ~ BD$COMPROMISOS + BD$SATISFACCION+BD$PERSONAL+BD$VOLVER.A.CONTRATAR+BD$PRODUCTOS+BD$SOPORTE_ADMON+BD$SOPORTE_DIR+BD$SOPORTE_TEC, doTrace=0,maxRuns=500)
j= 1
#print(c("CORRIDA",i))
#print(m_bor_r$finalDecision)
  while(j<=length(m_bor_r$finalDecision)){
    temp <- data.frame(a=names(m_bor_r$finalDecision)[j],b=as.character(m_bor_r$finalDecision)[j],c=i)
    #print(c("VARIABLE",j))
    j=j+1
    bortotr<- union_all(bortotr,temp)
  }
i=i+1
}
#GUARDAR RESULTADOS EJECUCIÓN BORUTA
write.csv(bortotr, file = "E:/PROYECTO/Documentos/PAYC/PROYECTO 10. REPLANIFICACIÓN PAYC/ANALISIS/OTRO SÍS, LA REC. DE CLI. Y LA SAT.  DE LOS CLI/SCRIPTS/BORUTA_MULT_V1.csv")




