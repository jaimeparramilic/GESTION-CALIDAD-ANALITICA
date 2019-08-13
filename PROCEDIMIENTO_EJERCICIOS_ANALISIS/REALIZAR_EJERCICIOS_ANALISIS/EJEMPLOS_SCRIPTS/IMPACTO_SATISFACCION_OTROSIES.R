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
indice <- createDataPartition(BD$OTRO_SIESCAT , p = .67,
                                  list = FALSE,
                                  times = 1)
BD_ENTRENO <-BD[indice,]
BD_CALIBRACION <-BD[-indice,]


#VARIABLE RETORNADO
#1. MÉTODO RANDOM FOREST
set.seed(3457)
m_rf_r <-randomForest(as.factor(BD$OTRO_SIESCAT) ~ BD$COMPROMISOS + BD$SATISFACCION+BD$PERSONAL+BD$VOLVER.A.CONTRATAR+BD$PRODUCTOS+BD$SOPORTE_ADMON+BD$SOPORTE_DIR+BD$SOPORTE_TEC, importance=TRUE,ntree=1000)
error_rf_r<-mean(m_rf_r$err.rate[,1])
imp_rf_r<-m_rf_r$importance[,3]
m_rf_r$importance[,3]
plot(m_rf_r, log="y")

#2. IMPORTANCIA RELATIVA -> REGRESIÓN LOGISTICA
m_rl_r_e<-glm(as.factor(OTRO_SIESCAT)  ~ COMPROMISOS + SATISFACCION+PERSONAL+VOLVER.A.CONTRATAR+PRODUCTOS+SOPORTE_ADMON+SOPORTE_DIR+SOPORTE_TEC,family=binomial(),data=BD_ENTRENO)
m_rl_r<-glm(as.factor(BD$OTRO_SIESCAT) ~ BD$COMPROMISOS + BD$SATISFACCION+BD$PERSONAL+BD$VOLVER.A.CONTRATAR+BD$PRODUCTOS+BD$SOPORTE_ADMON+BD$SOPORTE_DIR+BD$SOPORTE_TEC,family=binomial())
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
while(i<=100){
set.seed(as.numeric(Sys.time()))
m_bor_r <-Boruta(as.factor(BD$OTRO_SIESCAT) ~ BD$COMPROMISOS + BD$SATISFACCION+BD$PERSONAL+BD$VOLVER.A.CONTRATAR+BD$PRODUCTOS+BD$SOPORTE_ADMON+BD$SOPORTE_DIR+BD$SOPORTE_TEC, doTrace=0,maxRuns=500)
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
write.csv(bortotr, file = "E:/PROYECTO/Documentos/PAYC/PROYECTO 10. REPLANIFICACIÓN PAYC/ANALISIS/OTRO SÍS, LA REC. DE CLI. Y LA SAT.  DE LOS CLI/SCRIPTS/BORUTA_CANTIDAD_OTROSI_V1.csv")

#ARBOL DE REGRESION
fit1 <- rpart(as.factor(BD$OTRO_SIESCAT) ~ BD$COMPROMISOS + BD$PERSONAL+BD$VOLVER.A.CONTRATAR+BD$SOPORTE_TEC, 
              method="class",control=rpart.control(minsplit=10, cp=0.001))

summary(fit1) # detailed summary of splits
#GRAFICAR
plot(fit1, uniform=TRUE,  compress = TRUE
)
text(fit1,  use.n=TRUE,all=TRUE, cex=.7,fancy=FALSE,pretty=NULL)


post(fit1, file = "E:/PROYECTO/Documentos/PAYC/PROYECTO 10. REPLANIFICACIÓN PAYC/ANALISIS/OTRO SÍS, LA REC. DE CLI. Y LA SAT.  DE LOS CLI/SCRIPTS/ARBOL_OTROSIES.ps", 
     title = "", pretty = TRUE,
     use.n = TRUE, horizontal = TRUE)
prp(fit1,type=4,extra=2,cex=.5,gap=0,space=0,box.palette = "RdYlGn")

error_arbol_final <- fit1$cptable[which.min(fit1$cptable[,"xerror"]),"xerror"]* fit1$frame[1, 'dev']/fit1$frame[1, 'n']


