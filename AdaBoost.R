#------------------------------------------------------------------#
#                               LIBRARIAS                          #
#------------------------------------------------------------------#
library(ggplot2)
library(ggpubr)
library(reshape2)
library(dplyr)
library(tidyr)
library(caret)
library(purrr)
library(precrec)
library(ROSE)
library(adabag)

#------------------------------------------------------------------#
#                       CARGANDO LA BASE DE DATOS                  #
#------------------------------------------------------------------#
Datos<-read.delim("clipboard")
head(Datos,10)
str(Datos)



# Bias Varianza TRade-OFF
# -----------------------------------------
bias_variance<-function(df_X, df_Y, model, name.model, size_train , random){
  set.seed(random)
  training.samples <- df_Y %>% createDataPartition(p = size_train, list = FALSE)
  train_Y <- df_Y[training.samples]
  test_Y  <- df_Y[-training.samples]
  train_X <- df_X[training.samples, ]
  test_X  <- df_X[-training.samples, ]
  train_data=data.frame(cbind(train_Y,train_X)) ; names(train_data)=c("Y",names(train_X))
  train_errors =rep(0,10)
  test_errors = rep(0,10)
  rango=round(seq(1,10)*0.1*dim((train_X))[1],0)
  for (m in rango){
    train_errors[match(m,rango)] <-1-mean(predict(train(Y ~., data=train_data[1:m,], method = model$method,
                                                        trControl = trainControl("cv",2)),
                                                  train_X[1:m,],type = "raw")==train_Y[1:m])
    test_errors[match(m,rango)] <- 1-mean(predict(train(Y ~., data=train_data[1:m,],method = model$method,
                                                        trControl = trainControl("cv",2)),
                                                  test_X,type = "raw")==test_Y)}
  x  <- seq(10, 100, 10)
  df <- data.frame(x,train_errors,test_errors)
  df2 <- melt(data = df, id.vars = "x")
  ggplot(data = df2, aes(x = x, y = value, colour = variable)) + geom_line(size=1) + geom_point(size=3) +
    scale_color_manual(values = c("blue","orange")) + 
    labs(title=paste0("Evaluation Bias Variance Trade-off Model ",name.model))+
    scale_y_continuous(name="Error", limits=c(0, 0.45))+ 
    scale_x_continuous(name ="Training set size in Percent",breaks=x) +
    theme(legend.position = c(.95, .95),legend.justification = c("right", "top"),legend.title=element_blank())
}


#------------------------------------------------------------------#
#               DATOS DE ENTRENAMIENTO Y DE PRUEBA                 #
#------------------------------------------------------------------#
RNGkind(sample.kind = "Rounding")
set.seed(100)
indice<- createDataPartition(Datos$Approved, p=0.7, list=FALSE)
data.train <- Datos[ indice, ]                         
data.test  <- Datos[-indice, ] 
dim(data.train)
dim(data.test)


#------------------------------------------------------------------#
#                      EL ALGORITMO ADABOOST                       #
#------------------------------------------------------------------#
set.seed(200)
ctrl <- trainControl(method="cv", number=10)

modelo_boos <- train(Approved ~ ., 
                     data = data.train, 
                     method = "AdaBoost.M1", tuneLength=1,
                     trControl=ctrl, metric="Accuracy")

plot(modelo_boos)
pred_boos <- predict(modelo_boos, newdata=data.test[,-1],type="raw")
confusionMatrix(data= pred_boos, reference= as.factor(data.test$Approved), positive="Si")

# Predicci?n correcta y error
# ---------------------------
accuracy <- mean(data.test$Approved==pred_boos) ; accuracy
error <- mean(data.test$Approved!=pred_boos) ; error

# Curva ROC
# -----------
pred_boos2 <- predict(modelo_boos, newdata=data.test[,-1],type="prob")
roc.curve(data.test$Approved, pred_boos2[,2],lty=2,lwd=1.8,col="blue" ,main="ROC curves")