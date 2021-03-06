##Modelado
# Model 007. Cambiando las vbles categoricas en 0,1 dummys y Linear Regression 

#01. Cargar las librer�as necesarias
require(data.table)
require(dplyr)
require(ModelMetrics)
require(lars)

#02. Fijar la semilla
set.seed(1234)

#03. Creaci�n de una funci�n con el modelo y sus hiperpar�metros
trainLRModel <- function(train.data, train.target) {
  lr.model <- lm(train.target~., data=train.data)
  return(lr.model)
}
trainLASSOModel <- function(train.data, train.target) {
    laa<- lars(as.matrix(train.data),as.matrix(train.target),type = 'lasso')
    return(laa)
}

#04. Cargar el conjunto de datos original y seleccionar los trabajadores a tiempo completo
load("dat/data.rds")

#Selecciono solo los trabajadores a tiempo completo
data <- data %>%
  filter(`TIPOJORTIEMPO COMPLETO` == 1) %>%
  select(-c(`TIPOJORTIEMPO COMPLETO`,`TIPOJORTIEMPO PARCIAL`)) 


#05. Selecciono solo los hombres
data.h <- data %>%
  filter(SEXOHOMBRE == 1) %>%
  select(-c(SEXOMUJER,SEXOHOMBRE)) 


#06. Seleccionar el conjunto de train y test
numberOfSamples<- round(nrow(data.h) * .7)
train.data.h <- data.h[1:numberOfSamples,]
test.data.h<-data.h[-(1:numberOfSamples),]
dim(train.data.h)
dim(test.data.h)

train.data <- as.data.table(train.data.h)
test.data <- as.data.table(test.data.h)

#07. Dividimos entre data y target
train.target <- train.data[["SALANUAL"]]
train.data[, SALANUAL := NULL]


#08. Entrenamos el modelo 
lr.model.007 <- trainLRModel(train.data, train.target)  

#load("models/lr.model.007")
summary(lr.model.007)

#09. Graficamos el error
png("models/Importance.007.png")
layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))
plot(lr.model.007)
dev.off()

#10. Guardar el modelo
save(lr.model.007, file = "models/lr.model.007")

#11. Predecimos con los datos de test
dim(test.data)
head(test.data[,13])
pred.lr.007 <- predict(lr.model.007, test.data[,-13])
#?predict.gbm

#load("models/laa.model.007")

#12. Entrenamos el modelo laa
laa.model.007<- trainLASSOModel(train.data,train.target)

#13. Graficamos el modelo
plot(laa.model.007)

#14. Guardar el modelo
save(laa.model.007, file = "models/laa.model.007")

best_step<- laa.model.007$df[which.min(laa.model.007$Cp)]

#15. Predecimos con los datos de test
pred.laa.007<- predict.lars(laa.model.007,newx =as.matrix(test.data[,-13]), s=best_step, type= "fit")

pred.laa.007 <- as.vector(pred.laa.007$fit)


#16. Calculamos los errores de estimaci�n
#Raiz del error cuadr�tico medio de los logaritmos
rmse(log(test.data$SALANUAL),log(pred.laa.007))
# 0.4386815
#Ra�z del error cuadr�tico medio
rmse(test.data$SALANUAL, pred.laa.007)
# 27524.47
#Error cuadr�tico medio
mse(test.data$SALANUAL, pred.laa.007)
# 757596489
#Error medio absoluto
mae(test.data$SALANUAL, pred.laa.007)
# 12147.88


#17. Calculamos los errores de estimaci�n
#Raiz del error cuadr�tico medio de los logaritmos
rmse(log(test.data$SALANUAL),log(pred.lr.007))
# 
#Ra�z del error cuadr�tico medio
rmse(test.data$SALANUAL, pred.lr.007)
# 27474.62
#Error cuadr�tico medio
mse(test.data$SALANUAL, pred.lr.007)
# 754854624
#Error medio absoluto
mae(test.data$SALANUAL, pred.lr.007)
# 11994.81
