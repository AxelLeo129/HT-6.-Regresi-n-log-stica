library(caret)
library(dummies)
setwd("D:\AxelFolder\University\mineria_de_datos\HT-6.-Regresi-n-log-stica")

#Modelo de Regresi???n log???stica
porcentaje<-0.7
datos<-iris
set.seed(123)

datos<-cbind(datos,dummy(datos$Species,verbose = T))


corte <- sample(nrow(datos),nrow(datos)*porcentaje)
train<-datos[corte,]
test<-datos[-corte,]


#Queremos saber si una planta es virginica o no
modelo<-glm(datosvirginica~., data = train[,c(1:4,8)],family = binomial(), maxit=100)

#-------------------------------------------------
# Regresi???n Logistica 
#-------------------------------------------------

##Modelo con todas las variables
pred<-predict(modelo,newdata = test[,1:4], type = "response")
prediccion<-ifelse(pred>=0.5,1,0)
confusionMatrix(as.factor(test$datosvirginica),as.factor(prediccion))