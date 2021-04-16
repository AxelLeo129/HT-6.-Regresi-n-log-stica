#<<<<<<< HEAD
#setwd("C:/Users/Kevin Macario/Desktop/Uvg/9no Semestre/Mineria de Datos/HT-6.-Regresi-n-log-stica/house-prices-advanced-regression-techniques")
#datatest <- read.csv('test.csv')
#datatrain <- read.csv('train.csv')
#prices <- read.csv('sample_submission.csv')
#=======
library(caret)
library(dummies)
setwd("D:/AxelFolder/University/mineria_de_datos/HT-6.-Regresi-n-log-stica")
#>>>>>>> c524c2669884e3fb364a8e078a53e10658a44119

#setwd("C:/Users/LENOVO/Desktop/Clases/Minería de datos/Github/HT-6.-Regresi-n-log-stica")

porcentaje<-0.8
set.seed(666)

datatest <- read.csv("house-prices-advanced-regression-techniques/test.csv")
prices <- read.csv("house-prices-advanced-regression-techniques/sample_submission.csv")
datatrain <- read.csv("house-prices-advanced-regression-techniques/train.csv")

datatestc = merge(x = datatest, y = prices, by = "Id")


datos = rbind(datatestc, datatrain)
colSums(is.na(datos))
datos$PoolQC<-NULL
datos$Fence <- NULL
datos$MiscFeature<-NULL
datos$FireplaceQu<-NULL
datos$Alley<-NULL
datos<- datos[complete.cases(datos), ]
mask <- unlist(lapply(datos, is.numeric))
datos_numericos <- datos[,mask]
cluster<-kmeans(datos_numericos ,3, iter.max = 200 )
datos$grupo <- cluster$cluster

summary(datos$SalePrice[datos$grupo==1])
summary(datos$SalePrice[datos$grupo==2])
summary(datos$SalePrice[datos$grupo==3])

c1 <- rgb(173,216,230,max = 255, alpha = 80, names = "lt.blue")
c2 <- rgb(255,192,203, max = 255, alpha = 80, names = "lt.pink")
c3 <- rgb(120,160,180, max = 255, alpha = 80, names = "lt.hola")
break1 = floor(sqrt(nrow(datos[datos$grupo ==1,])))
break2 = floor(sqrt(nrow(datos[datos$grupo ==2,])))
break3 = floor(sqrt(nrow(datos[datos$grupo ==3,])))

histo1 <- hist(datos$SalePrice[datos$grupo ==1], breaks  = break1,plot = FALSE )
histo2 <- hist(datos$SalePrice[datos$grupo ==2],breaks = break2, plot = FALSE )
histo3 <- hist(datos$SalePrice[datos$grupo ==3],breaks = break3, plot = FALSE )
plot(histo1, c = c1, xlim = c(4000, 500000 ), ylim = c(0,200), main = "Histogramas 3 clústers", xlab ="Precios")
plot(histo2, c = c2,add = TRUE)
plot(histo3, c = c3,add = TRUE)

###################

datos<-cbind(datos,dummy(datos$grupo), verbose = T)
mask <- unlist(lapply(datos, is.numeric))
corte <- sample(nrow(datos),nrow(datos)*porcentaje)
train<-datos[corte,mask]
test<-datos[-corte,mask]

################# 

t <- proc.time()
model<-glm(as.factor(train$datos1)~., data = train[,c(1:38,40)],family = binomial(), maxit=100)
pred<-predict(model,newdata = test[,1:38], type = "response")
prediccion<-ifelse(pred>=0.5,1,0)
confusionMatrix(as.factor(test$datos1),as.factor(prediccion))
proc.time()-t

t <- proc.time()
pred<-predict(model,newdata = train[,1:38], type = "response")
prediccion<-ifelse(pred>=0.5,1,0)
confusionMatrix(as.factor(train$datos1),as.factor(prediccion))
proc.time()-t


t <- proc.time()
model<-glm(as.factor(train$datos2)~., data = train[,c(1:38,41)],family = binomial(), maxit=100)
pred<-predict(model,newdata = test[,1:38], type = "response")
prediccion<-ifelse(pred>=0.5,1,0)
confusionMatrix(as.factor(test$datos2),as.factor(prediccion))
proc.time()-t

t <- proc.time()
pred<-predict(model,newdata = train[,1:38], type = "response")
prediccion<-ifelse(pred>=0.5,1,0)
confusionMatrix(as.factor(train$datos2),as.factor(prediccion))
proc.time()-t


t <- proc.time()
model<-glm(as.factor(train$datos3)~., data = train[,c(1:38,42)],family = binomial(), maxit=100)
pred<-predict(model,newdata = test[,1:38], type = "response")
prediccion<-ifelse(pred>=0.5,1,0)
confusionMatrix(as.factor(test$datos3),as.factor(prediccion))
proc.time()-t

t <- proc.time()
pred<-predict(model,newdata = train[,1:38], type = "response")
prediccion<-ifelse(pred>=0.5,1,0)
confusionMatrix(as.factor(train$datos3),as.factor(prediccion))
proc.time()-t

