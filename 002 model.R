##Modelado
head(EES_2014)
# Model 002. Cambiando las vbles categoricas en 0,1 y randomForest con 100 arboles

require(data.table)
require(randomForest)
require(xgboost) # for xgboost
require(tidyverse)
require(dplyr)
require(data.table)
require(ModelMetrics)


set.seed(1234)

trainRFModel <- function(train.data, train.target, ntrees) {
  rf.model <- randomForest(train.data, y = train.target, 
                           ntree = ntrees,
                           importance=TRUE,
                           do.trace = T, na.action = na.omit)
  return(rf.model)
}


#Cargo el conjunto de datos
load("dat/data.rds")

#Selecciono solo los hombres
data.h <- data %>%
    filter(SEXOHOMBRE == 1) %>%
    select(-c(SEXOMUJER,SEXOHOMBRE)) 

dim(data.h)

#Selecciono conjunto de train y test

numberOfSamples<- round(nrow(data.h) * .7)
train.data.h <- data.h[1:numberOfSamples,]
test.data.h<-data.h[-(1:numberOfSamples),]
dim(train.data.h)
dim(test.data.h)

train.data <- as.data.table(train.data.h)
test.data <- as.data.table(test.data.h)

# Generamos las features
#train.data <- DataPipeline(train.data)
#test.data  <- DataPipeline(test.data)

train.target <- train.data[["SALANUAL"]]
train.data[, SALANUAL := NULL]

# Entrenamos un modelo 
rf.model.002 <- trainRFModel(train.data, train.target, 100)  

load("models/rf.model.002")
#
png("models/Importance.002.png")
varImpPlot(rf.model.002)
dev.off()

#
png("models/Error.002.png")
plot(rf.model.002)
dev.off()

save(rf.model.002, file = "models/rf.model.002")

# Predecimos
dim(test.data)
head(test.data[,13])
pred.rf.002 <- predict(rf.model.002, test.data[,-13], type = "response")
#?predict.randomForest



rmse(log(test.data$SALANUAL),log(pred.rf.002))
# 0.4389457
rmse(test.data$SALANUAL, pred.rf.002)
# 25007.82
mse(test.data$SALANUAL, pred.rf.002)
# 625390831
mae(test.data$SALANUAL, pred.rf.002)
# 9946,875
