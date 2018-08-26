##Modelado
# Model 001. RandomForest con 100 arboles y vbles sin modificar

#01. Cargar las librerías necesarias
require(data.table)
require(randomForest)
require(dplyr)
require(ModelMetrics)
require(tidyverse)
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
load("dat/EES_2014_v2.rds")

#05.Selección de variables 
#Selecciono las variables y quito las que no aportan informacion
#Tambien quito las variables que están relacionadas con el salario
#Cambiamos las variables de tipo character a factor
train.data <- EES_2014_v2 %>%
  select(-c(ORDENCCC, ORDENTRA)) %>%
  select(-c(ANOANTI,MESANTI,FIJODISM,FIJODISD,DRELABM,SIESPM1,DSIESPM1,SIESPM2,DSIESPM2,SALBASE,
            EXTRAORM,PHEXTRA,COMSAL,COMSALTT,IRPFMES,COTIZA,BASE,DRELABAM,DRELABAD,
            SALBRUTO,GEXTRA,VESP,FACTOTAL,DIASANO,DES_CNACE,DES_TCNO)) %>%
  mutate_if(is.character, as.factor)

#06. Segmentación del dataset en solo los hombres
data.h <- train.data %>%
  filter(SEXO == "HOMBRE")

dim(data.h)

#07. Seleccionar el conjunto de train y test
numberOfSamples<- round(nrow(data.h) * .7)
train.data.h <- data.h[1:numberOfSamples,]
test.data.h<-data.h[-(1:numberOfSamples),]
dim(train.data.h)
dim(test.data.h)

train.data <- as.data.table(train.data.h)
test.data <- as.data.table(test.data.h)

#08. Dividimos entre data y target
train.target <- train.data[["SALANUAL"]]
train.data[, SALANUAL := NULL]

dim(train.data)
dim(test.data)

#09. Entrenamos el modelo 
rf.model.001 <- trainRFModel(train.data, train.target, 100)  

#load("models/rf.model.001")

#10. Graficamos la evolución del error
mse <- data.frame(mse = rf.model.001$mse,
                      arboles = seq_along(rf.model.001$mse))

png("models/Error.001.png")
ggplot(data = mse, aes(x = arboles, y = mse )) +
  geom_line() +
  labs(title = "Evolución del error vs número árboles",
       x = "nº árboles") +
  theme_bw()
dev.off()

#11. Calcular la importancia de las variables
importancia_pred <- as.data.frame(importance(rf.model.001, scale = TRUE))
head(importancia_pred)
importancia_pred <- rownames_to_column(importancia_pred, var = "variable")
p1 <- ggplot(data = importancia_pred, aes(x = reorder(variable, `%IncMSE`),
                                          y = `%IncMSE`,
                                          fill = `%IncMSE`)) +
  labs(x = "variable", title = "Reducción de MSE") +
  geom_col() +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "bottom")

p2 <- ggplot(data = importancia_pred, aes(x = reorder(variable, IncNodePurity),
                                          y = IncNodePurity,
                                          fill = IncNodePurity)) +
  labs(x = "variable", title = "Reducción de pureza") +
  geom_col() +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "bottom")

png("models/Importance.001.png")
ggarrange(p1, p2)
dev.off()

#12. Guardar el modelo
save(rf.model.001, file = "models/rf.model.001")


#13. Predecimos con los datos de test
head(test.data)
head(test.data[,31])
pred.rf.001 <- predict(rf.model.001, test.data[,-31], type = "response")

#14. Calculamos los errores de estimación
#Raiz del error cuadrático medio de los logaritmos
rmse(log(test.data$SALANUAL),log(pred.rf.001))
#0.5229496
#Raíz del error cuadrático medio
rmse(test.data$SALANUAL, pred.rf.001)
# 25272.55
#Error cuadrático medio
mse(test.data$SALANUAL, pred.rf.001)
# 638701789
#Error medio absoluto
mae(test.data$SALANUAL, pred.rf.001)
# 10621.86

