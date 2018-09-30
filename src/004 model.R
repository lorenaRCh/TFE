##Modelado
# Model 004. Cambiando las vbles categoricas en 0,1 y randomForest con 500 arboles

#01. Cargar las librerías necesarias
require(data.table)
require(randomForest)
require(xgboost) # for xgboost
require(tidyverse)
require(dplyr)
require(data.table)
require(ModelMetrics)
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

train.data <- as.data.table(train.data.h)
test.data <- as.data.table(test.data.h)

colnames(train.data) <- c("VAL",
                          "VAN",      
                          "PUENTES",      
                          "JAP",      
                          "JSP1",      
                          "JSP2" ,     
                          "HEXTRA",      
                          "DSIESPA1",      
                          "DSIESPA2" ,     
                          "DSIESPA3"  ,    
                          "DSIESPA4"   ,   
                          "DIASRELABA"  ,    
                          "SALANUAL"     , 
                          "MESESANTIG"    ,  
                          "FIJODISDIAS"    ,  
                          "ESTU1"      ,
                          "ESTU2"      ,
                          "ESTU3"      ,
                          "ESTU4"      ,
                          "ESTU5",
                          "ESTU6"      ,
                          "ESTU7",      
                          "ANOS21"      ,
                          "ANOS22"      ,
                          "ANOS23"      ,
                          "ANOS24"      ,
                          "ANOS25"      ,
                          "ANOS26"      ,
                          "REGULACION4"      ,
                          "REGULACION3",      
                          "REGULACION1"      ,
                          "REGULACION5",      
                          "REGULACION2",      
                          "MERCADO1"      ,
                          "MERCADO4"      ,
                          "MERCADO2"      ,
                          "MERCADO3"  ,    
                          "ESTRATO21"      ,
                          "ESTRATO22"      ,
                          "ESTRATO23"      ,
                          "ESTRATO24"      ,
                          "ESTRATO25"      ,
                          "NUTS1CANARIAS"      ,
                          "NUTS1CENTRO"      ,
                          "NUTS1COMMADRID"  ,    
                          "NUTS1ESTE"      ,
                          "NUTS1NORESTE"    ,  
                          "NUTS1NOROESTE"    ,  
                          "NUTS1SUR"      ,
                          "CNACEB0"      ,
                          "CNACEC1"      ,
                          "CNACEC2"      ,
                          "CNACEC3"      ,
                          "CNACEC4"      ,
                          "CNACEC5"      ,
                          "CNACEC6"      ,
                          "CNACEC7"      ,
                          "CNACEC8"      ,
                          "CNACED0"      ,
                          "CNACEE0"      ,
                          "CNACEF0"      ,
                          "CNACEG1"      ,
                          "CNACEG2"      ,
                          "CNACEH1"      ,
                          "CNACEH2"      ,
                          "CNACEI0"      ,
                          "CNACEJ0"      ,
                          "CNACEK0"      ,
                          "CNACEL0"      ,
                          "CNACEM0"      ,
                          "CNACEN0"      ,
                          "CNACEO0"      ,
                          "CNACEP0"      ,
                          "CNACEQ0"      ,
                          "CNACER0"      ,
                          "CNACES0"      ,
                          "CNO1A0"      ,
                          "CNO1B0"      ,
                          "CNO1C0"      ,
                          "CNO1D0"      ,
                          "CNO1E0"      ,
                          "CNO1F0"      ,
                          "CNO1G0"      ,
                          "CNO1H0"      ,
                          "CNO1I0"      ,
                          "CNO1J0"      ,
                          "CNO1K0"      ,
                          "CNO1L0"      ,
                          "CNO1M0"      ,
                          "CNO1N0"      ,
                          "CNO1O0"      ,
                          "CNO1P0"      ,
                          "CNO1Q0"      ,
                          "CONTROLPRIVADO",      
                          "CONTROLPUBLICO" ,     
                          "TIPOPAISESPAÑA"  ,    
                          "TIPOPAISRESTOMUNDO",      
                          "TIPOCONDURACIONDETERMINADA",      
                          "TIPOCONDURACIONINDEFINIDA"  ,    
                          "RESPONSANO"      ,
                          "RESPONSASI")

colnames(test.data) <- c("VAL",
                          "VAN",      
                          "PUENTES",      
                          "JAP",      
                          "JSP1",      
                          "JSP2" ,     
                          "HEXTRA",      
                          "DSIESPA1",      
                          "DSIESPA2" ,     
                          "DSIESPA3"  ,    
                          "DSIESPA4"   ,   
                          "DIASRELABA"  ,    
                          "SALANUAL"     , 
                          "MESESANTIG"    ,  
                          "FIJODISDIAS"    ,  
                          "ESTU1"      ,
                          "ESTU2"      ,
                          "ESTU3"      ,
                          "ESTU4"      ,
                          "ESTU5",
                          "ESTU6"      ,
                          "ESTU7",      
                          "ANOS21"      ,
                          "ANOS22"      ,
                          "ANOS23"      ,
                          "ANOS24"      ,
                          "ANOS25"      ,
                          "ANOS26"      ,
                          "REGULACION4"      ,
                          "REGULACION3",      
                          "REGULACION1"      ,
                          "REGULACION5",      
                          "REGULACION2",      
                          "MERCADO1"      ,
                          "MERCADO4"      ,
                          "MERCADO2"      ,
                          "MERCADO3"  ,    
                          "ESTRATO21"      ,
                          "ESTRATO22"      ,
                          "ESTRATO23"      ,
                          "ESTRATO24"      ,
                          "ESTRATO25"      ,
                          "NUTS1CANARIAS"      ,
                          "NUTS1CENTRO"      ,
                          "NUTS1COMMADRID"  ,    
                          "NUTS1ESTE"      ,
                          "NUTS1NORESTE"    ,  
                          "NUTS1NOROESTE"    ,  
                          "NUTS1SUR"      ,
                          "CNACEB0"      ,
                          "CNACEC1"      ,
                          "CNACEC2"      ,
                          "CNACEC3"      ,
                          "CNACEC4"      ,
                          "CNACEC5"      ,
                          "CNACEC6"      ,
                          "CNACEC7"      ,
                          "CNACEC8"      ,
                          "CNACED0"      ,
                          "CNACEE0"      ,
                          "CNACEF0"      ,
                          "CNACEG1"      ,
                          "CNACEG2"      ,
                          "CNACEH1"      ,
                          "CNACEH2"      ,
                          "CNACEI0"      ,
                          "CNACEJ0"      ,
                          "CNACEK0"      ,
                          "CNACEL0"      ,
                          "CNACEM0"      ,
                          "CNACEN0"      ,
                          "CNACEO0"      ,
                          "CNACEP0"      ,
                          "CNACEQ0"      ,
                          "CNACER0"      ,
                          "CNACES0"      ,
                          "CNO1A0"      ,
                          "CNO1B0"      ,
                          "CNO1C0"      ,
                          "CNO1D0"      ,
                          "CNO1E0"      ,
                          "CNO1F0"      ,
                          "CNO1G0"      ,
                          "CNO1H0"      ,
                          "CNO1I0"      ,
                          "CNO1J0"      ,
                          "CNO1K0"      ,
                          "CNO1L0"      ,
                          "CNO1M0"      ,
                          "CNO1N0"      ,
                          "CNO1O0"      ,
                          "CNO1P0"      ,
                          "CNO1Q0"      ,
                          "CONTROLPRIVADO",      
                          "CONTROLPUBLICO" ,     
                          "TIPOPAISESPAÑA"  ,    
                          "TIPOPAISRESTOMUNDO",      
                          "TIPOCONDURACIONDETERMINADA",      
                          "TIPOCONDURACIONINDEFINIDA"  ,    
                          "RESPONSANO"      ,
                          "RESPONSASI")


#08. Entrenamos el modelo 
rf.model.004 <- trainRangerModel(train.data, 500)  

#load("models/rf.model.004")


#09. Calcular la importancia de las variables
importancia_pred <- as.data.frame(importance(rf.model.004, scale = TRUE))
importancia_pred <- rownames_to_column(importancia_pred, var = "variable")
colnames(importancia_pred) <- c("variable", "importance")
importancia_pred <-importancia_pred %>%
                       arrange(desc(importance))
importancia_pred1 <- head(importancia_pred,10)

png("models/Importance.004.png")
ggplot(data = importancia_pred1, aes(x = reorder(variable, importance),
                                    y = importance,
                                    fill = importance)) +
  labs(x = "variable", title = "Importance") +
  geom_col() +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "bottom")
dev.off()


#11. Guardar el modelo
save(rf.model.004, file = "models/rf.model.004")

#12. Predecimos con los datos de test
dim(test.data)
head(test.data[,13])
pred.rf.004 <- predict(rf.model.004, test.data[,-13], type = "response")


#13. Calculamos los errores de estimación
#Raiz del error cuadrático medio de los logaritmos
rmse(log(test.data$SALANUAL),log(pred.rf.004$predictions))
# 0.4010418
#Raíz del error cuadrático medio
rmse(test.data$SALANUAL, pred.rf.004$predictions)
#Error cuadrático medio
# 26711.95
mse(test.data$SALANUAL, pred.rf.004$predictions)
# 713528245
#Error medio absoluto
mae(test.data$SALANUAL, pred.rf.004$predictions)
# 10974.02

