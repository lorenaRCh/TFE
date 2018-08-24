##02 Preparacion datos solo numericos con variables dummys en las variables categoricas 

require(dplyr)

#Cargo la tabla que hemos preparado en el paso 01
load("dat/EES_2014_v2.rds")

##01. Transformación de variables

#Convertimos la información categorica a numérica a través de one-hot-encoding
#Nivel de estudios
EES_estudios <- model.matrix(~ESTU-1,EES_2014_v2)
#Rango Edad
EES_edad <- model.matrix(~ANOS2-1,EES_2014_v2)
#Regulacion
EES_regulacion <- model.matrix(~REGULACION-1,EES_2014_v2)
#Mercado
EES_mercado <- model.matrix(~MERCADO-1,EES_2014_v2)
#Numero Empleados
EES_numempleados <- model.matrix(~ESTRATO2-1,EES_2014_v2)
#Zona
EES_zona <- model.matrix(~NUTS1-1,EES_2014_v2)
#CNAE
EES_cnae <- model.matrix(~CNACE-1,EES_2014_v2)
#Ocupacion
EES_ocupacion <- model.matrix(~CNO1-1,EES_2014_v2)
#Sexo
EES_sexo <- model.matrix(~SEXO-1,EES_2014_v2)
#Control
EES_control <- model.matrix(~CONTROL-1,EES_2014_v2)
#Nacionalidad
EES_nacionalidad <- model.matrix(~TIPOPAIS-1,EES_2014_v2)
#Jornada
EES_jornada <- model.matrix(~TIPOJOR-1,EES_2014_v2)
#Contrato
EES_contrato <- model.matrix(~TIPOCON-1,EES_2014_v2)
#Responsabilidad/Supervisión
EES_responsabilidad <- model.matrix(~RESPONSA-1,EES_2014_v2)
######SituacionesEspeciales
EES_2014_v2$SIESPM1<-ifelse(EES_2014_v2$SIESPM1=="SI",1, ifelse(EES_2014_v2$SIESPM1=="NO",0,""))
EES_2014_v2$SIESPM2<-ifelse(EES_2014_v2$SIESPM2=="SI",1, ifelse(EES_2014_v2$SIESPM2=="NO",0,""))
EES_2014_v2$SIESPA1<-ifelse(EES_2014_v2$SIESPA1=="SI",1, ifelse(EES_2014_v2$SIESPA1=="NO",0,""))
EES_2014_v2$SIESPA2<-ifelse(EES_2014_v2$SIESPA2=="SI",1, ifelse(EES_2014_v2$SIESPA2=="NO",0,""))
EES_2014_v2$SIESPA3<-ifelse(EES_2014_v2$SIESPA3=="SI",1, ifelse(EES_2014_v2$SIESPA3=="NO",0,""))
EES_2014_v2$SIESPA4<-ifelse(EES_2014_v2$SIESPA4=="SI",1, ifelse(EES_2014_v2$SIESPA4=="NO",0,""))

##02.Selección de Variables

#Selecciono las variables y quito las que no aportan informacion
#Tambien quito las variables que están relacionadas con el salario
train.data <- EES_2014_v2 %>%
  select(-c(ORDENCCC, ORDENTRA)) %>%
  select(-c(ANOANTI,MESANTI,FIJODISM,FIJODISD,DRELABM,SIESPM1,DSIESPM1,SIESPM2,DSIESPM2,SALBASE,
            EXTRAORM,PHEXTRA,COMSAL,COMSALTT,IRPFMES,COTIZA,BASE,DRELABAM,DRELABAD,
            SALBRUTO,GEXTRA,VESP,FACTOTAL,DIASANO,DES_CNACE,DES_TCNO)) %>%
  select_if(is.numeric)

data<-cbind(train.data,EES_estudios,EES_edad,EES_regulacion,EES_mercado,EES_numempleados,EES_zona,EES_cnae,
            EES_ocupacion, EES_sexo,EES_control,EES_nacionalidad,EES_jornada,EES_contrato,EES_responsabilidad)


save(data, file = "dat/data.rds")

