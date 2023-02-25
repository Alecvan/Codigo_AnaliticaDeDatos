#------------------------------------------------------------------#
#                               LIBRARIAS                          #
#------------------------------------------------------------------#
library(ggplot2)
library(reshape)
library(ggpubr)
library(reshape2)
library(dplyr)
library(caret)
library(rpart)
library(rpart.plot)
library(purrr)
library(precrec)
library(ROSE)
library(ROCR)
#------------------------------------------------------------------#
#                       CARGANDO LA BASE DE DATOS                  #
#------------------------------------------------------------------#
Datos<-read.delim("clipboard")
head(Datos,10)
str(Datos)

#------------------------------------------------------------------#
#               DATOS DE ENTRENAMIENTO Y DE PRUEBA                 #
#------------------------------------------------------------------#

set.seed(1983)
indice<- createDataPartition(Datos$Approved, p=0.7, list=FALSE)
data.train <- Datos[ indice, ]                         
data.test  <- Datos[-indice, ] 
dim(data.train)
dim(data.test)


#------------------------------------------------------------------#
#                   EL ALGORITMO RANDOM FOREST                     #
#------------------------------------------------------------------#

set.seed(200)
modelLookup(model='rf')
ctrl <- trainControl(method="cv",number=10)

modelo_rf <- train(Approved ~ ., 
                   data = data.train, 
                   method = "rf", 
                   trControl = ctrl, 
                   tuneLength = 5,
                   metric="Accuracy")


modelo_rf

pred_rf <- predict(modelo_rf, newdata=data.test[,-1],type="raw")
confusionMatrix(data= pred_rf, reference= as.factor(data.test$Approved), positive="Si")

# Prediccion correcta y error
# ---------------------------
accuracy <- mean(data.test$Approved==pred_rf) ; accuracy
error <- mean(data.test$Approved!=pred_rf) ; error

# Curva ROC
# -----------
pred_rf2 <- predict(modelo_rf, newdata=data.test[,-1],type="prob")
roc.curve(data.test$Approved, pred_rf2[,2],lty=2,lwd=1.8,col="blue" ,main="ROC curves")

