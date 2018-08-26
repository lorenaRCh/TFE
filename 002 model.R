##Modelado
# Model 002. Cambiando las vbles categoricas en 0,1 y randomForest con 100 arboles

#01. Cargar las librerías necesarias
require(data.table)
require(randomForest)
require(xgboost) # for xgboost
require(tidyverse)
require(dplyr)
require(data.table)
require(ModelMetrics)
require(ggpubr)

#02. Fijar la semilla
set.seed(1234)

#03. Creación de una función con el modelo y sus hiperparámetros
trainRFModel <- function(train.data, train.target, ntrees) {
  rf.model <- randomForest(train.data, y = train.target, 
                           ntree = ntrees,
                           importance=TRUE,
                           do.trace = T, na.action = na.omit)
  return(rf.model)
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

train.data <- as.data.table(train.data.h)
test.data <- as.data.table(test.data.h)

#07. Dividimos entre data y target
train.target <- train.data[["SALANUAL"]]
train.data[, SALANUAL := NULL]


#08. Entrenamos el modelo 
rf.model.002 <- trainRFModel(train.data, train.target, 100)  

#load("models/rf.model.002")


#09. Graficamos la evolución del error
mse <- data.frame(mse = rf.model.002$mse,
                  arboles = seq_along(rf.model.002$mse))

png("models/Error.002.png")
ggplot(data = mse, aes(x = arboles, y = mse )) +
  geom_line() +
  labs(title = "Evolución del error vs número árboles",
       x = "nº árboles") +
  theme_bw()
dev.off()


#10. Calcular la importancia de las variables
importancia_pred <- as.data.frame(importance(rf.model.002, scale = TRUE))
importancia_pred <- rownames_to_column(importancia_pred, var = "variable")
importancia_pred <-importancia_pred %>%
                       arrange(desc(importancia_pred$`%IncMSE`))
importancia_pred1 <- head(importancia_pred,10)
importancia_pred <-importancia_pred %>%
                       arrange(desc(importancia_pred$IncNodePurity))

importancia_pred2 <- head(importancia_pred,10)

p1 <- ggplot(data = importancia_pred1, aes(x = reorder(variable, `%IncMSE`),
                                          y = `%IncMSE`,
                                          fill = `%IncMSE`)) +
  labs(x = "variable", title = "Reducción de MSE") +
  geom_col() +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "bottom")

p2 <- ggplot(data = importancia_pred2, aes(x = reorder(variable, IncNodePurity),
                                          y = IncNodePurity,
                                          fill = IncNodePurity)) +
  labs(x = "variable", title = "Reducción de pureza") +
  geom_col() +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "bottom")

png("models/Importance.002.png")
ggarrange(p1, p2)
dev.off()


#11. Guardar el modelo
save(rf.model.002, file = "models/rf.model.002")

#12. Predecimos con los datos de test
dim(test.data)
head(test.data[,13])
pred.rf.002 <- predict(rf.model.002, test.data[,-13], type = "response")


#13. Calculamos los errores de estimación
#Raiz del error cuadrático medio de los logaritmos
rmse(log(test.data$SALANUAL),log(pred.rf.002))
# 0.4330174
#Raíz del error cuadrático medio
rmse(test.data$SALANUAL, pred.rf.002)
#Error cuadrático medio
# 24860.77
mse(test.data$SALANUAL, pred.rf.002)
# 618057968
#Error medio absoluto
mae(test.data$SALANUAL, pred.rf.002)
# 9811.499

