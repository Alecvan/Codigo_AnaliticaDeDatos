#==================================================================#
#          REGRESIÓN LOGÍSTICA binaria                             # 
#==================================================================#
cc_approvals<-read.delim("clipboard")
cc_approvals$Approved <- as.factor(cc_approvals$Approved)
head(cc_approvals)
str(cc_approvals)
#------------------------------------------------------------------#
#                             Prueba Total                         #
#------------------------------------------------------------------#
library(rms)

#Estadísico Omnibus y estadístico de Wald
#---------------------
lrm(formula = Approved ~ .,scale=T,data=cc_approvals)

#------------------------------------------------------------------#
#                      Elección del modelo final                   #
#------------------------------------------------------------------#
cc_approvals.m1<-glm(Approved ~ ., family = binomial(link = logit),data=cc_approvals)
summary(cc_approvals.m1)

#  Selección de Variables  
step <- stepAIC(cc_approvals.m1,direction="backward", trace=FALSE)
step$anova


# Formulando el modelo sin las variables no significativas
lrm(formula = Approved ~ Married + BankCustomer + Ethnicity + PriorDefault + 
      Employed + CreditScore + Citizen + ZipCode + Income,scale=T,data=cc_approvals)

cc_approvals.m2<-glm(Approved ~ Married + BankCustomer + Ethnicity + PriorDefault + 
                       Employed + CreditScore + Citizen + ZipCode + Income, family = binomial(link = logit),data=cc_approvals)
summary(cc_approvals.m2)

anova(cc_approvals.m2,cc_approvals.m1,test = "Chisq")

#----------------------------------------------------------
# Cociente de ventajas (OR) e IC 95% 
library(MASS)
exp(cbind(OR = coef(cc_approvals.m2),confint.default(cc_approvals.m2)))

#----------------------------------------------------------
#------------------------------------------------------------------#
#    Prueba de Hosmer y Lemeshow para adecuadión del modelo        #
#------------------------------------------------------------------#
library(generalhoslem)
logitgof(cc_approvals$Approved, fitted(cc_approvals.m2))

#------------------------------------------------------------------#
#    R2-medidas de bondad de ajuste y prueba del modelo            #
#------------------------------------------------------------------#
library(rcompanion)
nagelkerke(cc_approvals.m2)


#  Probabilidades y grupo estimadas 
#------------------------------------------------------------------#
#              Probabilidades y grupo estimadas                    #
#------------------------------------------------------------------#
proba.pred=predict(cc_approvals.m2,type="response")
proba.pred
clase.pred <- ifelse(proba.pred >= 0.5, 1, 0)
finaldata = cbind(cc_approvals, proba.pred,clase.pred)
ggplot(finaldata, aes(x = proba.pred, fill = Approved)) + geom_density(alpha = 0.5)
#------------------------------------------------------------------#
#                      Tabla de clasificación                      #
#------------------------------------------------------------------#

# Calcular el accuracy
#-----------------------
accuracy <- mean(cc_approvals$Approved==clase.pred)
accuracy

# Calcular el error de mala clasificación
#----------------------------------------
error <- mean(cc_approvals$Approved!=clase.pred)
error

library(caret)
confusionMatrix(as.factor(clase.pred),cc_approvals$Approved,positive="1")

# Curva Roc
# ----------
library(ROSE)
roc.curve(cc_approvals$Approved, proba.pred,lty=2,lwd=1.8,col="blue" ,main="ROC curves")
