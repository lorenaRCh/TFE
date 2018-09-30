##Aplicar Mejor modelo al dataset Femenino

#01. Cargar las librerías necesarias
#require(data.table)
require(xgboost)
require(tidyverse)
require(ModelMetrics)

#02. Fijar la semilla
set.seed(1234)

#03. Cargar el conjunto de datos original y seleccionar los trabajadores a tiempo completo
load("dat/data.rds")

#Selecciono solo los trabajadores a tiempo completo
data <- data %>%
  filter(`TIPOJORTIEMPO COMPLETO` == 1) %>%
  select(-c(`TIPOJORTIEMPO COMPLETO`,`TIPOJORTIEMPO PARCIAL`)) 


#05. Selecciono solo los hombres
data.m <- data %>%
  filter(SEXOMUJER == 1) %>%
  select(-c(SEXOMUJER,SEXOHOMBRE)) 

#06. Dividimos entre data y target y transformamos a matrix
# y convertimos la variable objetivo en logaritmo
datam.target <- log10(data.m[["SALANUAL"]])
datam.data <-data.m %>%
  select(-SALANUAL)
datam.matrix <- data.matrix(datam.data)

#07. Convertimos a DMatrix
ddatam <- xgb.DMatrix(data = datam.matrix, label= datam.target)

#08. Cargamos el modelo seleccionado
load("models/xgb.model.013")

#09. Predecimos con los datos de test
pred.muj.013 <- predict(xgb.model.013, ddatam)
#head(pred.xgb.013)
pred.muj.013 <- 10^(pred.muj.013)

#10. Calculamos los errores de estimación
#Raiz del error cuadrático medio de los logaritmos
rmse(log(data.m$SALANUAL),log(pred.muj.013))
# 0.336136
#Raíz del error cuadrático medio
rmse(data.m$SALANUAL, pred.muj.013)
# 12402.49
#Error cuadrático medio
mse(data.m$SALANUAL, pred.muj.013)
# 153821763
#Error medio absoluto
mae(data.m$SALANUAL, pred.muj.013)
# 7520.982

#11. Cargamos los datos originales
load("dat/EES_2014_v2.rds")
EES_2014_V2

fem.data <- EES_2014_v2 %>%
  filter(TIPOJOR == "TIEMPO COMPLETO" & SEXO == "MUJER")

fem.data <- cbind(fem.data, SALESTIMADO=pred.muj.013)

save(fem.data, file = "dat/fem.data.rds")

