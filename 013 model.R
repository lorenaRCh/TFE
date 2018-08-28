##Modelado
# Model 013. Cambiando las vbles categoricas en 0,1 dummys y xgboost con gbtree 500 y mejores parametros
# con el logaritmo de la vble objetivo

#01. Cargar las librerías necesarias
#require(data.table)
require(xgboost)
require(tidyverse)
require(ModelMetrics)

#02. Fijar la semilla
set.seed(1234)

#03. Creación de una función para encontrar los mejores hiperparámetros
ParametrosXGBoost<- function(n, cv.nround, cv.nfold) {
  All_rmse<- c()
  Param_group<-c()
  for (iter in 1:n) {
    param <- list(objective = "reg:linear",
                  eval_metric = "rmse",
                  booster = "gbtree",
                  max_depth = sample(6:10, 1),
                  eta = runif(1, 0.01, 0.3),
                  gamma = runif(1, 0.0, 0.2), 
                  subsample = runif(1, 0.6, 0.9),
                  colsample_bytree = runif(1, 0.5, 0.8)
                  
    )
    #cv.nround = 100
    #cv.nfold = 4
    mdcv <- xgb.cv(data=dtrain, params = param, nthread=6, 
                   nfold=cv.nfold, 
                   nrounds=cv.nround,
                   verbose = TRUE)
    # Least Mean_Test_RMSE as Indicator # 
    # Least Mean_Test_RMSE as Indicator # 
    min_rmse<- min(mdcv$evaluation_log$test_rmse_mean)
    All_rmse<-append(All_rmse,min_rmse)
    Param_group<-append(Param_group,param)
    # Select Param
    param<-Param_group[(which.min(All_rmse)*8+1):(which.min(All_rmse)*8+8)]
  }
  return(param)
}

#04. Creación de una función con el modelo y sus hiperparámetros
trainXGBoostModel<- function(dmatrix, nrounds, param) {
  xgb.model <- xgboost(params=param,
                       data = dmatrix, 
                       verbose = TRUE,
                       nrounds = nrounds)
  return(xgb.model)
}

#05. Cargar el conjunto de datos original
load("dat/data.rds")

#06. Selecciono solo los hombres
data.h <- data %>%
  filter(SEXOHOMBRE == 1) %>%
  select(-c(SEXOMUJER,SEXOHOMBRE)) 


#07. Seleccionar el conjunto de train y test
numberOfSamples<- round(nrow(data.h) * .7)
train.data.h <- data.h[1:numberOfSamples,]
test.data.h<-data.h[-(1:numberOfSamples),]


#08. Dividimos entre data y target y transformamos a matrix
# y convertimos la variable objetivo en logaritmo
train.target <- log(train.data.h[["SALANUAL"]])
train.data <-train.data.h %>%
  select(-SALANUAL)
train.matrix <- data.matrix(train.data)

# Seleccionamos en test Target y data y pasamos a matrix
test.target <- log(test.data.h[["SALANUAL"]])
test.data <-test.data.h %>%
  select(-SALANUAL)
test.matrix <- data.matrix(test.data)

#09. Convertimos a DMatrix
dtrain <- xgb.DMatrix(data = train.matrix, label= train.target)
dtest <- xgb.DMatrix(data = test.matrix, label= test.target)

#10. Calculamos los mejores hiperparametros
param500 <- ParametrosXGBoost(20, 500, 4)


#11. Entrenamos el modelo 
xgb.model.013 <- trainXGBoostModel(dtrain, 500, param500)  

xgb.model.013
#load("models/xgb.model.013")

mat <- xgb.importance (feature_names = colnames(dtrain),model = xgb.model.013)

#12. Graficamos la importancia de las variables
importance_matrix = mat[1:20]
png("models/Importance.013.png")
ggplot(data = importance_matrix, aes(x = reorder(Feature, Gain),
                                     y = Gain,
                                     fill = Gain)) +
  geom_col() +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "bottom")
dev.off()

#13. Guardar el modelo
save(xgb.model.013, file = "models/xgb.model.013")

#14. Predecimos con los datos de test
pred.xgb.013 <- predict(xgb.model.013, dtest)
#head(pred.xgb.013)
pred.xgb.013 <- exp(pred.xgb.013)

#15. Calculamos los errores de estimación
#Raiz del error cuadrático medio de los logaritmos
rmse(log(test.data.h$SALANUAL),log(pred.xgb.013))
# 0.3858326
#Raíz del error cuadrático medio
rmse(test.data.h$SALANUAL, pred.xgb.013)
# 24465.1
#Error cuadrático medio
mse(test.data.h$SALANUAL, pred.xgb.013)
# 598541017
#Error medio absoluto
mae(test.data.h$SALANUAL, pred.xgb.013)
# 8763.387



#$`objective`
#[1] "reg:linear"
#$eval_metric
#[1] "rmse"
#$booster
#[1] "gbtree"
#$max_depth
#[1] 6
#$eta
#[1] 0.2019541
#$gamma
#[1] 0.01430493
#$subsample
#[1] 0.8933691
#$colsample_bytree
#[1] 0.7517879