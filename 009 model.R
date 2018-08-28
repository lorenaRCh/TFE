##Modelado
# Model 009. Cambiando las vbles categoricas en 0,1 dummys y xgboost con gblinear 

#01. Cargar las librerías necesarias
#require(data.table)
require(xgboost)
require(tidyverse)
require(ModelMetrics)

#02. Fijar la semilla
set.seed(1234)

#03. Creación de una función con el modelo y sus hiperparámetros
trainXGBoostModelL <- function(dmatrix, nrounds) {
  xgb.model <- xgboost(data = dmatrix, objective = "reg:linear",
                       eval_metric = "rmse",
                       booster = "gblinear",
                       verbose = TRUE,
                       nrounds = nrounds)
  return(xgb.model)
}


#04. Cargar el conjunto de datos original
load("dat/data.rds")

#05. Selecciono solo los hombres
data.h <- data %>%
  filter(SEXOHOMBRE == 1) %>%
  select(-c(SEXOMUJER,SEXOHOMBRE)) 


#06. Seleccionar el conjunto de train y test
numberOfSamples<- round(nrow(data.h) * .7)
train.data.h <- data.h[1:numberOfSamples,]
test.data.h<-data.h[-(1:numberOfSamples),]


#07. Dividimos entre data y target y transformamos a matrix
train.target <- train.data.h[["SALANUAL"]]
train.data <-train.data.h %>%
  select(-SALANUAL)
train.matrix <- data.matrix(train.data)

# Seleccionamos en test Target y data y pasamos a matrix
test.target <- test.data.h[["SALANUAL"]]
test.data <-test.data.h %>%
  select(-SALANUAL)
test.matrix <- data.matrix(test.data)

#08. Convertimos a DMatrix
dtrain <- xgb.DMatrix(data = train.matrix, label= train.target)
dtest <- xgb.DMatrix(data = test.matrix, label= test.target)

#09. Entrenamos el modelo  
xgb.model.009 <- trainXGBoostModelL(dtrain, 100)  

#load("models/xgb.model.009")

mat <- xgb.importance (feature_names = colnames(dtrain),model = xgb.model.009)

#10. Graficamos la importancia de las variables
importance_matrix = mat[1:20]
head(importance_matrix)
png("models/Importance.009.png")
ggplot(data = importance_matrix, aes(x = reorder(Feature, Weight),
                                     y = Weight,
                                     fill = Weight)) +
  geom_col() +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "bottom")
dev.off()

#11. Guardar el modelo
save(xgb.model.009, file = "models/xgb.model.009")

#12. Predecimos con los datos de test
pred.xgb.009 <- predict(xgb.model.009, dtest)
head(pred.xgb.009)

#13. Calculamos los errores de estimación
#Raiz del error cuadrático medio de los logaritmos
rmse(log(test.data.h$SALANUAL),log(pred.xgb.009))
# NANs produced
#Raíz del error cuadrático medio
rmse(test.data.h$SALANUAL, pred.xgb.009)
# 25397.72
#Error cuadrático medio
mse(test.data.h$SALANUAL, pred.xgb.009)
# 645044168
#Error medio absoluto
mae(test.data.h$SALANUAL, pred.xgb.009)
# 10395.91


