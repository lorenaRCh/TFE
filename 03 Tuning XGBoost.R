library(xgboost)
library(Matrix)
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

ParametrosXGBoost(10,500,4)

´  