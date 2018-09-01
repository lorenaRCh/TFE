##Modelado
# Model 005. gbm con 1000 arboles y vbles sin modificar

#01. Cargar las librerías necesarias
require(data.table)
require(gbm)
require(dplyr)
require(ModelMetrics)


#02. Fijar la semilla
set.seed(1234)

#03. Creación de una función con el modelo y sus hiperparámetros
trainGBMModel <- function(train.data, train.target, ntrees, depth) {
  gbm.model <- gbm.fit(as.data.frame(train.data), y = as.vector(train.target), 
                   ,distribution = "gaussian",n.trees = ntrees, #, cv.folds=10,
                   shrinkage = 0.1,
                   n.minobsinnode=20,
                   bag.fraction=0.5
                   , interaction.depth = depth, verbose = T)
  return(gbm.model)
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


#06. Selecciono solo los hombres
data.h <- train.data %>%
  filter(SEXO == "HOMBRE")%>%
  select(-c(SEXO))


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


#09. Entrenamos el modelo  
gbm.model.005 <- trainGBMModel(train.data, train.target, 1000, 5)  


#load("models/gbm.model.005")

#10. Graficamos la importancia de las variables 
importancia_pred <- as.data.frame(summary(gbm.model.005))
importancia_pred <- rownames_to_column(importancia_pred, var = "variable")
png("models/Importance.005.png")
ggplot(data = importancia_pred, aes(x = reorder(variable, rel.inf),
                                    y = rel.inf,
                                    fill = rel.inf)) +
  labs(x = "variable", title = "Importancia") +
  geom_col() +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "bottom")
dev.off()

#11. Guardar el modelo
save(gbm.model.005, file = "models/gbm.model.005")

#12. Graficamos la evolución del error
n.trees = seq(from=100 ,to=1000, by=100) #no of trees-a vector of 100 values 

pred <- predict(gbm.model.005,test.data[,-31],n.trees = 1000, type = "response")

#Generating a Prediction matrix for each Tree
predmatrix<-predict(gbm.model.005,test.data[,-31],n.trees = n.trees)
dim(predmatrix) #dimentions of the Prediction 

test.error<-with(test.data,apply( (predmatrix-SALANUAL)^2,2,mean))
head(test.error) #contains the Mean squared test error for each of the 100 trees averaged

#Plotting the test error vs number of trees
plot(n.trees , test.error , pch=19,col="blue",xlab="Nº árboles",ylab="Test Error", main = "Evolución del error vs Número de árboles")

png("models/Error.005.png")
plot(n.trees , test.error , pch=19,col="blue",xlab="Nº árboles",ylab="Test Error", main = "Evolución del error vs Número de árboles")
dev.off()

#13. Predecimos con los datos de test
dim(test.data)
head(test.data[,29])
pred.gbm.005 <- predict(gbm.model.005, test.data[,-29], n.tree = 1000, type = "response")
#?predict.gbm

pred.gbm.005 <- as.vector(pred.gbm.005)

#14. Calculamos los errores de estimación
#Raiz del error cuadrático medio de los logaritmos
rmse(log(test.data$SALANUAL),log(pred.gbm.005))
#
#Raíz del error cuadrático medio
rmse(test.data$SALANUAL, pred.gbm.005)
# 26861.62
#Error cuadrático medio
mse(test.data$SALANUAL, pred.gbm.005)
# 721546874
#Error medio absoluto
mae(test.data$SALANUAL, pred.gbm.005)
# 11428.09
