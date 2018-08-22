##Modelado
# Model 003. RandomForest con 500 arboles y vbles sin modificar

require(data.table)
require(randomForest)
require(dplyr)
require(ModelMetrics)
set.seed(1234)

trainRFModel <- function(train.data, train.target, ntrees) {
  rf.model <- randomForest(train.data, y = train.target, 
                           ntree = ntrees,
                           importance=TRUE,
                           do.trace = T, na.action = na.omit)
  return(rf.model)
}

load("dat/EES_2014_v2.rds")

#Selecciono las variables y quito las que no aportan informacion
#Tambien quito las variables que están relacionadas con el salario
#Cambiamos las variables de tipo character a factor
train.data <- EES_2014_v2 %>%
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


train.target <- train.data[["SALANUAL"]]
train.data[, SALANUAL := NULL]
head(test.data)

# Entrenamos un modelo 
rf.model.003 <- trainRFModel(train.data, train.target, 500)  

load("models/rf.model.003")

png("models/Importance.003.png")
varImpPlot(rf.model.003)
dev.off()


png("models/Error.003.png")
plot(rf.model.003)

dev.off()


importancia_pred <- as.data.frame(importance(rf.model.003, scale = TRUE))
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

png("models/Reduccion.003.png")
ggarrange(p1, p2)
dev.off()

save(rf.model.003, file = "models/rf.model.003")

# Predecimos
dim(train.data)
head(train.data)
head(test.data[,31])
pred.rf.003 <- predict(rf.model.003, test.data[,-31], type = "response")


rmse(log(test.data$SALANUAL),log(pred.rf.003))
# 0.5221218
rmse(test.data$SALANUAL, pred.rf.003)
# 25257.75
mse(test.data$SALANUAL, pred.rf.003)
# 637954056
mae(test.data$SALANUAL, pred.rf.003)
# 10571.16


