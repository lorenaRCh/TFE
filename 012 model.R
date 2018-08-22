##Modelado
# Model 012. Cambiando las vbles categoricas en 0,1 dummys y xgboost con gbtree 500 y mejores parametros

#require(data.table)
require(xgboost)
require(tidyverse)
require(ModelMetrics)


set.seed(1234)
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

trainXGBoostModel<- function(dmatrix, nrounds, param) {
  xgb.model <- xgboost(params=param,
                       data = dmatrix, 
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

#Calculamos los mejores parametros
param500 <- ParametrosXGBoost(20, 500, 4)


# Entrenamos un modelo 
xgb.model.012 <- trainXGBoostModel(dtrain, 500, param500)  

#load("models/xgb.model.008")

mat <- xgb.importance (feature_names = colnames(dtrain),model = xgb.model.012)

png("models/Importance.012.png")
xgb.plot.importance (importance_matrix = mat[1:20])
dev.off()

save(xgb.model.012, file = "models/xgb.model.012")

# Predecimos
pred.xgb.012 <- predict(xgb.model.012, dtest)
#head(pred.xgb.012)

rmse(log(test.data.h$SALANUAL),log(pred.xgb.012))
# 
rmse(test.data.h$SALANUAL, pred.xgb.012)
# 24601.71
mse(test.data.h$SALANUAL, pred.xgb.012)
# 605244197
mae(test.data.h$SALANUAL, pred.xgb.012)
# 9495.669


