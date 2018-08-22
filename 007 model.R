##Modelado
head(EES_2014)
# Model 007. Cambiando las vbles categoricas en 0,1 dummys y Linear Regression 

require(data.table)
require(dplyr)
require(ModelMetrics)
require(lars)


set.seed(1234)

trainLRModel <- function(train.data, train.target) {
  lr.model <- lm(train.target~., data=train.data)
  return(lr.model)
}
trainLASSOModel <- function(train.data, train.target) {
    laa<- lars(as.matrix(train.data),as.matrix(train.target),type = 'lasso')
    return(laa)
}





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
lr.model.007 <- trainLRModel(train.data, train.target)  

summary(lr.model.007)


#load("models/gbm.model.006")
png("models/Importance.007.png")
layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))
plot(lr.model.007)
dev.off()

# Predecimos
dim(test.data)
head(test.data[,13])
pred.lr.007 <- predict(lr.model.007, test.data[,-13])
#?predict.gbm


laa.model.007<- trainLASSOModel(train.data,train.target)
plot(laa.model.007)

save(laa.model.007, file = "models/laa.model.007")

best_step<- laa.model.007$df[which.min(laa.model.007$Cp)]
pred.laa.007<- predict.lars(laa.model.007,newx =as.matrix(test.data[,-13]), s=best_step, type= "fit")

pred.laa.007 <- as.vector(pred.laa.007$fit)

head(pred.laa.007)
rmse(log(test.data$SALANUAL),log(pred.laa.007))
# 0.5189894
rmse(test.data$SALANUAL, pred.laa.007)
# 25212.22
mse(test.data$SALANUAL, pred.laa.007)
# 635655811
# get the mean average error for our new model, based on our test data
mae(test.data$SALANUAL, pred.laa.007)
# 10710.03



