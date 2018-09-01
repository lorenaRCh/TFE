##Modelado
# Model 013. Cambiando las vbles categoricas en 0,1 dummys y xgboost con gbtree 500 y mejores parametros
# con el logaritmo de la vble objetivo

#01. Cargar las librerías necesarias
#require(data.table)
require(xgboost)
require(tidyverse)
require(ModelMetrics)

#02. Fijar la semilla
set.seed(1234)

#03. Creación de una función con el modelo y sus hiperparámetros
trainXGBoostModel<- function(dmatrix, nrounds) {
  xgb.model <- xgboost(objective = "reg:linear", 
                       eval_metric = "rmse", 
                       booster = "gbtree", 
                       max_depth =8, 
                       eta = 0.0701235727220774, 
                       gamma = 0.0680891748517752, 
                       subsample = 0.838016262394376, 
                       colsample_bytree = 0.531581114884466,
                       data = dmatrix, 
                       verbose = TRUE,
                       nrounds = nrounds)
  return(xgb.model)
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


#06. Seleccionar el conjunto de train y test
numberOfSamples<- round(nrow(data.h) * .7)
train.data.h <- data.h[1:numberOfSamples,]
test.data.h<-data.h[-(1:numberOfSamples),]


#07. Dividimos entre data y target y transformamos a matrix
# y convertimos la variable objetivo en logaritmo
train.target <- log10(train.data.h[["SALANUAL"]])
train.data <-train.data.h %>%
  select(-SALANUAL)
train.matrix <- data.matrix(train.data)

# Seleccionamos en test Target y data y pasamos a matrix
test.target <- log10(test.data.h[["SALANUAL"]])
test.data <-test.data.h %>%
  select(-SALANUAL)
test.matrix <- data.matrix(test.data)

#08. Convertimos a DMatrix
dtrain <- xgb.DMatrix(data = train.matrix, label= train.target)
dtest <- xgb.DMatrix(data = test.matrix, label= test.target)


#09. Entrenamos el modelo 
xgb.model.013 <- trainXGBoostModel(dtrain, 500)  

xgb.model.013
#load("models/xgb.model.013")

mat <- xgb.importance (feature_names = colnames(dtrain),model = xgb.model.013)

#10. Graficamos la importancia de las variables
importance_matrix = mat[1:20]
png("models/Importance.013.png")
ggplot(data = importance_matrix, aes(x = reorder(Feature, Gain),
                                     y = Gain,
                                     fill = Gain)) +
  geom_col() +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "bottom")
dev.off()

#11. Guardar el modelo
save(xgb.model.013, file = "models/xgb.model.013")

#12. Predecimos con los datos de test
pred.xgb.013 <- predict(xgb.model.013, dtest)
#head(pred.xgb.013)
pred.xgb.013 <- 10^(pred.xgb.013)

#13. Calculamos los errores de estimación
#Raiz del error cuadrático medio de los logaritmos
rmse(log(test.data.h$SALANUAL),log(pred.xgb.013))
# 0.3654682
#Raíz del error cuadrático medio
rmse(test.data.h$SALANUAL, pred.xgb.013)
# 26334.95
#Error cuadrático medio
mse(test.data.h$SALANUAL, pred.xgb.013)
# 693529504
#Error medio absoluto
mae(test.data.h$SALANUAL, pred.xgb.013)
# 9869.522

