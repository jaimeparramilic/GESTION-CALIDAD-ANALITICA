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
BD <- subset(BD,(SERVICIO=="INT"))
#Parición de los datos
set.seed(3457)
indice <- createDataPartition(BD$RETORNADO, p = .67,
                                  list = FALSE,
                                  times = 1)
BD_ENTRENO <-BD[indice,]
BD_CALIBRACION <-BD[-indice,]
#VARIABLE RETORNADO
#1. MÉTODO RANDOM FOREST
set.seed(348)
m_rf_r <-randomForest(as.factor(BD$RETORNADO_CATEGORIA) ~ BD$COMPROMISOS + BD$SATISFACCION+BD$PERSONAL+BD$VOLVER.A.CONTRATAR+BD$PRODUCTOS+BD$SOPORTE_ADMON+BD$SOPORTE_DIR+BD$SOPORTE_TEC, importance=TRUE,ntree=10000)
error_rf_r<-mean(m_rf_r$err.rate[,1])
imp_rf_r<-m_rf_r$importance[,3]
m_rf_r$importance[,3]

#2. IMPORTANCIA RELATIVA -> REGRESIÓN LOGISTICA
m_rl_r_e<-glm(as.factor(RETORNADO_CATEGORIA) ~ COMPROMISOS + SATISFACCION+PERSONAL+VOLVER.A.CONTRATAR+PRODUCTOS+SOPORTE_ADMON+SOPORTE_DIR+SOPORTE_TEC,family=binomial(),data=BD_ENTRENO)
m_rl_r<-glm(as.factor(BD$RETORNADO_CATEGORIA) ~ BD$COMPROMISOS + BD$SATISFACCION+BD$PERSONAL+BD$VOLVER.A.CONTRATAR+BD$PRODUCTOS+BD$SOPORTE_ADMON+BD$SOPORTE_DIR+BD$SOPORTE_TEC,family=binomial())
imp_rl_r <-varImp(m_rl_r, scale = FALSE)
summary(m_rl_r)
BD_CALIBRACION$pred_rl_r <-predict(m_rl_r_e, select(BD_CALIBRACION,COMPROMISOS , SATISFACCION,PERSONAL,VOLVER.A.CONTRATAR,PRODUCTOS,SOPORTE_ADMON,SOPORTE_DIR,SOPORTE_TEC), type = "response")
BD_CALIBRACION <- BD_CALIBRACION  %>% mutate(pred_rl_r1 = 1*(pred_rl_r > .53) + 0)
BD_CALIBRACION <- BD_CALIBRACION  %>% mutate(accurate = 1*(pred_rl_r1 == RETORNADO))
error_rl_r <- 1 - (sum(BD_CALIBRACION$accurate)/nrow(BD_CALIBRACION))
#3. IMPORTANCIA RELATIVA -> MARS -> NO APLICA
#4. REGRESIÓN LINEAL -> NO APLICA
#5. BORUTA
i=1
#warnings()
bortotr <- data.frame(a="",b="",c=0, stringsAsFactors=FALSE)
while(i<=20){
set.seed(as.numeric(Sys.time()))
m_bor_r <-Boruta(as.factor(BD$RETORNADO_CATEGORIA) ~ BD$COMPROMISOS + BD$SATISFACCION+BD$PERSONAL+BD$VOLVER.A.CONTRATAR+BD$PRODUCTOS+BD$SOPORTE_ADMON+BD$SOPORTE_DIR+BD$SOPORTE_TEC, doTrace=0,maxRuns=300)
j= 1
  while(j<=length(m_bor_r$finalDecision)){
    temp <- data.frame(a=names(m_bor_r$finalDecision)[j],b=as.character(m_bor_r$finalDecision)[j],c=i)
    
    j=j+1
    bortotr<- union_all(bortotr,temp)
  }
i=i+1
}
#GUARDAR RESULTADOS EJECUCIÓN BORUTA
write.csv(bortotr, file = "E:/PROYECTO/Documentos/PAYC/PROYECTO 10. REPLANIFICACIÓN PAYC/ANALISIS/OTRO SÍS, LA REC. DE CLI. Y LA SAT.  DE LOS CLI/SCRIPTS/BORUTA_RETORNADOS_INT.csv")




