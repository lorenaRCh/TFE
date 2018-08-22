##Modelado
head(EES_2014)
# Model 005. gbm con 1000 arboles y vbles sin modificar

require(data.table)
require(gbm)
require(dplyr)
require(ModelMetrics)

set.seed(1234)

trainGBMModel <- function(train.data, train.target, ntrees, depth) {
  gbm.model <- gbm.fit(as.data.frame(train.data), y = as.vector(train.target), 
                   ,distribution = "gaussian",n.trees = ntrees, #, cv.folds=10,
                   shrinkage = 0.01, interaction.depth = depth, verbose = T)
  return(gbm.model)
}


#Selecciono las variables y quito las que no aportan informacion
#Tambien quito las variables que están relacionadas con el salario
#Cambiamos las variables de tipo character a factor
train.data <- EES_2014 %>%
  select(-c(ORDENCCC, ORDENTRA)) %>%
  select(-c(ANOANTI,MESANTI,FIJODISM,FIJODISD,DRELABM,SIESPM1,DSIESPM1,SIESPM2,DSIESPM2,SALBASE,
            EXTRAORM,PHEXTRA,COMSAL,COMSALTT,IRPFMES,COTIZA,BASE,DRELABAM,DRELABAD,
            SALBRUTO,GEXTRA,VESP,FACTOTAL,DIASANO,DES_CNACE,DES_TCNO)) %>%
  mutate_if(is.character, as.factor)

head(train.data)

#Selecciono solo los hombres
data.h <- train.data %>%
  filter(SEXO == "HOMBRE")

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
#head(train.data)

# Entrenamos un modelo 
gbm.model.005 <- trainGBMModel(train.data, train.target, 1000, 4)  


#load("models/gbm.model.005")
png("models/Importance.005.png")
summary(gbm.model.005)
dev.off()


save(gbm.model.005, file = "models/gbm.model.005")

#######Miramos el numero de arboles
n.trees = seq(from=100 ,to=1000, by=100) #no of trees-a vector of 100 values 

pred <- predict(gbm.model.005,test.data[,-31],n.trees = 1000, type = "response")

#Generating a Prediction matrix for each Tree
predmatrix<-predict(gbm.model.005,test.data[,-31],n.trees = n.trees)
dim(predmatrix) #dimentions of the Prediction 

test.error<-with(test.data,apply( (predmatrix-SALANUAL)^2,2,mean))
head(test.error) #contains the Mean squared test error for each of the 100 trees averaged

#Plotting the test error vs number of trees
plot(n.trees , test.error , pch=19,col="blue",xlab="Number of Trees",ylab="Test Error", main = "Perfomance of Boosting on Test Set")

#adding the RandomForests Minimum Error line trained on same data and similar parameters
abline(h = min(test.error),col="red") #test.err is the test error of a Random forest fitted on same data
legend("topright",c("Minimum Test error Line for Random Forests"),col="red",lty=1,lwd=1)

png("models/Error.005.png")
plot(n.trees , test.error , pch=19,col="blue",xlab="Number of Trees",ylab="Test Error", main = "Perfomance of Boosting on Test Set")
dev.off()

# Predecimos
dim(test.data)
head(test.data[,31])
pred.gbm.005 <- predict(gbm.model.005, test.data[,-31], n.tree = 1000, type = "response")
#?predict.gbm

pred.gbm.005 <- as.vector(pred.gbm.005)

rmse(log(test.data$SALANUAL),log(pred.gbm.005))
# 0.5189894
rmse(test.data$SALANUAL, pred.gbm.005)
# 25212.22
mse(test.data$SALANUAL, pred.gbm.005)
# 635655811
# get the mean average error for our new model, based on our test data
mae(test.data$SALANUAL, pred.gbm.005)
# 10710.03
