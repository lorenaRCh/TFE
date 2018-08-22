##Modelado
# Model 008. Cambiando las vbles categoricas en 0,1 dummys y xgboost con gbtree

#require(data.table)
require(xgboost)
require(tidyverse)
require(ModelMetrics)


set.seed(1234)

trainXGBoostModel <- function(dmatrix, nrounds) {
  xgb.model <- xgboost(data = dmatrix, objective = "reg:linear",
                       eval_metric = "rmse",
                       booster = "gbtree",
                       verbose = TRUE,
                       nrounds = nrounds)
  return(xgb.model)
}

#Cargo el conjunto de datos
load("dat/data.rds")

#Selecciono solo los hombres
data.h <- data %>%
  filter(SEXOHOMBRE == 1) %>%
  select(-c(SEXOMUJER,SEXOHOMBRE)) 


#Selecciono conjunto de train y test
numberOfSamples<- round(nrow(data.h) * .7)
train.data.h <- data.h[1:numberOfSamples,]
test.data.h<-data.h[-(1:numberOfSamples),]


# Seleccionamos en train Target y data y pasamos a matrix
train.target <- train.data.h[["SALANUAL"]]
train.data <-train.data.h %>%
      select(-SALANUAL)
train.matrix <- data.matrix(train.data)

# Seleccionamos en test Target y data y pasamos a matrix
test.target <- test.data.h[["SALANUAL"]]
test.data <-test.data.h %>%
  select(-SALANUAL)
test.matrix <- data.matrix(test.data)

#Convertimos a DMatrix
dtrain <- xgb.DMatrix(data = train.matrix, label= train.target)
dtest <- xgb.DMatrix(data = test.matrix, label= test.target)


# Entrenamos un modelo 
xgb.model.008 <- trainXGBoostModel(dtrain, 100)  


#load("models/xgb.model.008")

mat <- xgb.importance (feature_names = colnames(dtrain),model = xgb.model.008)

png("models/Importance.008.png")
xgb.plot.importance (importance_matrix = mat[1:20])
dev.off()

save(xgb.model.008, file = "models/xgb.model.008")

# Predecimos
pred.xgb.008 <- predict(xgb.model.008, dtest)
#head(pred.xgb.008)

#Calculamos los errores
rmse(log(test.data.h$SALANUAL),log(pred.xgb.008))
# 
rmse(test.data.h$SALANUAL, pred.xgb.008)
# 24786.84
mse(test.data.h$SALANUAL, pred.xgb.008)
# 614387286
mae(test.data.h$SALANUAL, pred.xgb.008)
# 9734.581


