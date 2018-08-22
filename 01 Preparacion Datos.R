library("readxl")

#Cargo la tabla 
load("dat/EES_2014.rds")


#Realizo la transformacion de las vbles
EES_2014$NUTS1<-ifelse(EES_2014$NUTS1==1, "NOROESTE", ifelse(EES_2014$NUTS1==2, "NORESTE", ifelse(EES_2014$NUTS1==3, "COM. MADRID"
                                                                                                  , ifelse(EES_2014$NUTS1==4, "CENTRO", ifelse(EES_2014$NUTS1==5, "ESTE", ifelse(EES_2014$NUTS1==6, "SUR",
                                                                                                                                                                                 ifelse(EES_2014$NUTS1==7, "CANARIAS","")))))))

EES_2014$ESTRATO2<-ifelse(EES_2014$ESTRATO2==0, "1. INCLUYE TODOS LOS ESTRATOS", ifelse(EES_2014$ESTRATO2==1, "2.DE 1  A 49 TRABAJADORES", 
                                                                                        ifelse(EES_2014$ESTRATO2==2, "3.DE 50 A 199 TRABAJADORES", ifelse(EES_2014$ESTRATO2==3, "4.200 Y MAS TRABAJADORES", 
                                                                                                                                                          ifelse(EES_2014$ESTRATO2==4, "5.INCLUYE ESTRATO 2 Y 3","")))))

EES_2014$CONTROL<-ifelse(EES_2014$CONTROL==1,"PUBLICO", ifelse(EES_2014$CONTROL==2, "PRIVADO",""))

EES_2014$MERCADO<-ifelse(EES_2014$MERCADO==1,"LOCAL O REGIONAL", ifelse(EES_2014$MERCADO==2, "NACIONAL"
                                                                        , ifelse(EES_2014$MERCADO==3, "UNION EUROPEA", ifelse(EES_2014$MERCADO==4, "MUNDIAL",""))))

EES_2014$REGULACION<-ifelse(EES_2014$REGULACION==1,"ESTATAL SECTORIAL", ifelse(EES_2014$REGULACION==2, "SECTORIAL DE AMBITO INFERIOR"
                                                                              , ifelse(EES_2014$REGULACION==3, "DE EMPRESA O GRUPO DE EMPRESAS", ifelse(EES_2014$REGULACION==4, "DE CENTRO DE TRABAJO"
                                                                           , ifelse(EES_2014$REGULACION==5, "OTRA FORMA DE REGULACIÓN","")))))

EES_2014$SEXO<-ifelse(EES_2014$SEXO==1,"HOMBRE", ifelse(EES_2014$SEXO==6, "MUJER",""))

EES_2014$TIPOPAIS<-ifelse(EES_2014$TIPOPAIS==1,"ESPAÑA", ifelse(EES_2014$TIPOPAIS==2, "RESTO MUNDO",""))

EES_2014$TIPOJOR<-ifelse(EES_2014$TIPOJOR==1,"TIEMPO COMPLETO", ifelse(EES_2014$TIPOJOR==2, "TIEMPO PARCIAL",""))

EES_2014$TIPOCON<-ifelse(EES_2014$TIPOCON==1,"DURACION INDEFINIDA", ifelse(EES_2014$TIPOCON==2, "DURACION DETERMINADA",""))

EES_2014$SIESPM1<-ifelse(EES_2014$SIESPM1==1,"SI", ifelse(EES_2014$SIESPM1==6, "NO",""))

EES_2014$SIESPM2<-ifelse(EES_2014$SIESPM2==1,"SI", ifelse(EES_2014$SIESPM2==6, "NO",""))

EES_2014$SIESPA1<-ifelse(EES_2014$SIESPA1==1,"SI", ifelse(EES_2014$SIESPA1==6, "NO",""))

EES_2014$SIESPA2<-ifelse(EES_2014$SIESPA2==1,"SI", ifelse(EES_2014$SIESPA2==6, "NO",""))

EES_2014$SIESPA3<-ifelse(EES_2014$SIESPA3==1,"SI", ifelse(EES_2014$SIESPA3==6, "NO",""))

EES_2014$SIESPA4<-ifelse(EES_2014$SIESPA4==1,"SI", ifelse(EES_2014$SIESPA4==6, "NO",""))

EES_2014$ANOS2<-ifelse(EES_2014$ANOS2=="01","1.MENOS 19 AÑOS", ifelse(EES_2014$ANOS2=="02", "2.DE 20 A 29",
                                                                      ifelse(EES_2014$ANOS2=="03","3.DE 30 A 39", ifelse(EES_2014$ANOS2=="04", "4.DE 40 A 49",
                                                                                                                         ifelse(EES_2014$ANOS2=="05","5.DE 50 A 59", ifelse(EES_2014$ANOS2=="06", "6.MAS DE 59",""))))))

EES_2014$RESPONSA<-ifelse(EES_2014$RESPONSA==1,"SI", ifelse(EES_2014$RESPONSA==0, "NO",""))

EES_2014$ESTU<-ifelse(EES_2014$ESTU==1, "1.Menos que primaria", 
                      ifelse(EES_2014$ESTU==2, "2.Educacion primaria", 
                             ifelse(EES_2014$ESTU==3, "3.Primera etapa de educacion secundaria",
                                    ifelse(EES_2014$ESTU==4, "4.Segunda etapa de educacion secundaria", 
                                           ifelse(EES_2014$ESTU==5, "5.Enseñanzas de formación profesional de grado superior y similares", 
                                                  ifelse(EES_2014$ESTU==6, "6.Diplomados universitarios y similares",
                                                         ifelse(EES_2014$ESTU==7, "7.Licenciados y similares, y doctores universitarios","")))))))


# DÃ???as aÃ±o
#Multiplica los meses trabajados por los días del mes más la duración de la relación laboral en el año en días
EES_2014$DIASRELABA <- EES_2014$DRELABAM * 30.42 + EES_2014$DRELABAD
EES_2014$DIASRELABA[EES_2014$DIASRELABA > 365] <- 365
##Calcula dias trabajados al año con los dias de relación laboral restándole los días en situación especial 2 y 4
EES_2014$DIASANO <- EES_2014$DIASRELABA - EES_2014$DSIESPA2 - EES_2014$DSIESPA4


# Salario bruto anual 
# El salario bruto + la valoración en especie * la proporción de días trabajado
EES_2014$SALANUAL = (365/EES_2014$DIASANO)*(EES_2014$SALBRUTO + EES_2014$VESP)


#Cargo las dos tablas maestras de las variables CNAE y ocupacion
TCNO<-read_xlsx("dat/TCNO.xlsx")
TCNACE<-read_xlsx("dat/TCNACE.xlsx")

#Uno los descriptivos a la tabla original
EES_2014<-merge(EES_2014, TCNO, by="CNO1") 
EES_2014<-merge(EES_2014, TCNACE, by="CNACE") 

head(EES_2014)

#Creamos las variables MESESANTIG Y FIJODISDIAS
#Calculando la antiguedad en meses
EES_2014$MESESANTIG <- EES_2014$ANOANTI*12 + EES_2014$MESANTI
#Calculando el fijo discontinuo en dias
EES_2014$FIJODISDIAS <- EES_2014$FIJODISM*30.42 + EES_2014$FIJODISD

EES_2014_v2 <- EES_2014

save(EES_2014_v2, file = "dat/EES_2014_v2.rds")


