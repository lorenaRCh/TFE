##Modelado
# Model 001. RandomForest con 100 arboles y vbles sin modificar

#01. Cargar las librerías necesarias
require(data.table)
require(randomForest)
require(dplyr)
require(ModelMetrics)
require(tidyverse)
require(ggpubr)
require(ranger)


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

trainRangerModel <- function(train.data, ntrees) {
  rf.ranger <- ranger(SALANUAL ~ ., data = train.data, num.trees = ntrees, 
                      write.forest = TRUE, importance="permutation")
  return(rf.ranger)
}

#04. Cargar el conjunto de datos original
load("dat/EES_2014_v2.rds")

#05.Selección de variables 
#Selecciono las variables y quito las que no aportan informacion
#Tambien quito las variables que están relacionadas con el salario
#Cambiamos las variables de tipo character a factor
train.data <- EES_2014_v2 %>%
  filter(TIPOJOR == "TIEMPO COMPLETO") %>%
  select(-c(ORDENCCC, ORDENTRA)) %>%
  select(-c(ANOANTI,MESANTI,FIJODISM,FIJODISD,DRELABM,SIESPM1,DSIESPM1,SIESPM2,DSIESPM2,SALBASE,
            EXTRAORM,PHEXTRA,COMSAL,COMSALTT,IRPFMES,COTIZA,BASE,DRELABAM,DRELABAD,
            SALBRUTO,GEXTRA,VESP,FACTOTAL,DIASANO,DES_CNACE,DES_TCNO,TIPOJOR)) %>%
  mutate_if(is.character, as.factor)

head(train.data)

#06. Segmentación del dataset en solo los hombres
data.h <- train.data %>%
  filter(SEXO == "HOMBRE") %>%
  select(-c(SEXO))

dim(data.h)

#07. Seleccionar el conjunto de train y testnumberOfSamples<- round(nrow(data.h) * .7)
numberOfSamples<- round(nrow(data.h) * .7)
train.data.h <- data.h[1:numberOfSamples,]
test.data.h<-data.h[-(1:numberOfSamples),]
dim(train.data.h)
dim(test.data.h)

train.data <- as.data.table(train.data.h)
test.data <- as.data.table(test.data.h)


#08. Entrenamos el modelo 
rf.model.001 <- trainRangerModel(train.data, 100)
#load("models/rf.model.001")


#09. Calcular la importancia de las variables
importancia_pred <- as.data.frame(importance(rf.model.001, scale = TRUE))
importancia_pred <- rownames_to_column(importancia_pred, var="variable")
colnames(importancia_pred) <- c("variable", "importance")

png("models/Importance.001.png")
ggplot(data = importancia_pred, aes(x = reorder(variable, importance),
                                          y = importance,
                                          fill = importance)) +
  labs(x = "variable", title = "Importance") +
  geom_col() +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "bottom")
dev.off()

#10. Guardar el modelo
save(rf.model.001, file = "models/rf.model.001")

#11. Predecimos con los datos de test
head(test.data[,29])
pred.rf.001 <- predict(rf.model.001, test.data[,-29], type = "response")

#12. Calculamos los errores de estimación
#Raiz del error cuadrático medio de los logaritmos
rmse(log(test.data$SALANUAL),log(pred.rf.001$predictions))
# 0.4143569
#Raíz del error cuadrático medio
rmse(test.data$SALANUAL, pred.rf.001$predictions)
# 27131.72
#Error cuadrático medio
mse(test.data$SALANUAL, pred.rf.001$predictions)
# 736130049
#Error medio absoluto
mae(test.data$SALANUAL, pred.rf.001$predictions)
# 11580.4


