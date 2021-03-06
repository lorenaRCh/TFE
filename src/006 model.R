##Modelado
# Model 006. Cambiando las vbles categoricas en 0,1 dummys y gbm con 1000 arboles 

#01. Cargar las librer�as necesarias
require(data.table)
require(gbm)
require(dplyr)
require(ModelMetrics)

#02. Fijar la semilla
set.seed(1234)

#03. Creaci�n de una funci�n con el modelo y sus hiperpar�metros
trainGBMModel <- function(train.data, train.target, ntrees, depth) {
  gbm.model <- gbm.fit(as.data.frame(train.data), y = as.vector(train.target), 
                       ,distribution = "gaussian",n.trees = ntrees, #, cv.folds=10,
                       shrinkage = 0.1
                       , interaction.depth = depth,
                       n.minobsinnode=20,
                       bag.fraction=0.5, verbose = T)
  return(gbm.model)
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

dim(data.h)

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
gbm.model.006 <- trainGBMModel(train.data, train.target, 1000, 5)  


#load("models/gbm.model.006")


#09. Graficamos la importancia de las variables 
importancia_pred <- as.data.frame(summary(gbm.model.006))
importancia_pred <- rownames_to_column(importancia_pred, var = "variable")
importancia_pred <-importancia_pred %>%
  arrange(desc(importancia_pred$rel.inf))
importancia_pred <- head(importancia_pred,10)

png("models/Importance.006.png")
ggplot(data = importancia_pred, aes(x = reorder(variable, rel.inf),
                                    y = rel.inf,
                                    fill = rel.inf)) +
  labs(x = "variable", title = "Importancia") +
  geom_col() +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "bottom")
dev.off()

#10. Guardar el modelo
save(gbm.model.006, file = "models/gbm.model.006")

#11. Graficamos la evoluci�n del error
n.trees = seq(from=100 ,to=1000, by=100) #no of trees-a vector of 100 values 

head(test.data[,13])
pred <- predict(gbm.model.006,test.data[,-13],n.trees = 1000, type = "response")

#Generating a Prediction matrix for each Tree
predmatrix<-predict(gbm.model.006,test.data[,-13],n.trees = n.trees)
dim(predmatrix) #dimentions of the Prediction 

test.error<-with(test.data,apply( (predmatrix-SALANUAL)^2,2,mean))
head(test.error) #contains the Mean squared test error for each of the 100 trees averaged

png("models/Error.006.png")
plot(n.trees , test.error , pch=19,col="blue",xlab="N� �rboles",ylab="Test Error", main = "Evoluci�n del error vs N�mero de �rboles")
dev.off()

#12. Predecimos con los datos de test
dim(test.data)
head(test.data[,13])
pred.gbm.006 <- predict(gbm.model.006, test.data[,-13], n.tree = 1000, type = "response")


pred.gbm.006 <- as.vector(pred.gbm.006)

#13. Calculamos los errores de estimaci�n
#Raiz del error cuadr�tico medio de los logaritmos
rmse(log(test.data$SALANUAL),log(pred.gbm.006))
# --0.4057941
#Ra�z del error cuadr�tico medio
rmse(test.data$SALANUAL, pred.gbm.006)
# 26947.63
#Error cuadr�tico medio
mse(test.data$SALANUAL, pred.gbm.006)
# 726174740
#Error medio absoluto
mae(test.data$SALANUAL, pred.gbm.006)
# 11159.9
