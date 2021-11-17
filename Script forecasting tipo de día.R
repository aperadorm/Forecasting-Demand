#Primeramente es necesario descargar las siguientes librerias:
install.packages("tseries")
install.packages("forecast")
install.packages("lubridate")
install.packages("astsa")
install.packages("tidyverse")
install.packages("foreign")
install.packages("quantmod")
install.packages("aTSA")
install.packages("deSolve")
install.packages("Knitr")

#Corremos las librerias necesarias
library(readxl)
library(dplyr)
library(tseries)
library(forecast)
library(fpp2)
library(lubridate)
library(tidyr)
library(astsa)
library(tidyverse)
library(foreign)
library(quantmod)
library(deSolve)
library(knitr)


#Subir base de datos de XM demanda de energía SIN (GWh)
data <- read_excel("~/Downloads/Trabajo Pronóstico demanda SIN /data.xlsx")
View(data)
# Subir base de datos festivos en Colombia
Festivos_ <- read_excel("~/Downloads/Trabajo Pronóstico demanda SIN /Festivos .xlsx")
View(Festivos_)

#Cambiamos el formato a fecha 
data$fecha <- as.Date(paste(data$`Tiempo - Año` ,data$`Tiempo - Mes`,
                            data$`Tiempo - Día`,sep = "-"),format= "%Y-%m-%d")

#Agregamos columna que nos muestre el día
data$dia <- weekdays(data$fecha) 

#Unir base de datos (data) con (Festivos_)

names(Festivos_)[2] <- "tipo"
names(Festivos_)[1] <- "fecha"
data <- data %>% left_join(Festivos_, by="fecha")

#Catalogamos los tipos de día, Lunes a viernes= ORDINARIO, sabado= SÁBADO, domingo y festivos= FESTIVO

names(Festivos_) <- c("fecha", "tipo")

data$tipo2[data["dia"] == "Monday" & is.na(data["tipo"]) == T] <- "ORDINARIO"
data$tipo2[data["dia"] == "Tuesday"& is.na(data["tipo"]) == T] <- "ORDINARIO"
data$tipo2[data["dia"] == "Wednesday"& is.na(data["tipo"]) == T] <- "ORDINARIO"
data$tipo2[data["dia"] == "Thursday"& is.na(data["tipo"]) == T] <- "ORDINARIO"
data$tipo2[data["dia"] == "Friday"& is.na(data["tipo"]) == T] <- "ORDINARIO"
data$tipo2[data["dia"] == "Saturday"& is.na(data["tipo"]) == T] <- "SÁBADO"
data$tipo2[data["dia"] == "Sunday"& is.na(data["tipo"]) == T] <- "FESTIVO"

data$tipo2[data["tipo"] == "FESTIVO"] <- "FESTIVO"

#Organizamos la base de datos y cambiamos los nombres 
data <-data [,-7]
names(data)[1] <- "AÑO"
names(data)[2] <- "MES"
names(data)[3] <- "DIA"
names(data)[7] <- "Tipo"
names(data)[4] <-"DEMANDA_SIN"


#Cambiamos AÑO Y MES de chr a num
data$AÑO <- as.numeric(data$AÑO)
data$MES<- as.numeric(data$MES)

######################################################################################

#Ahora se calcula la demanda mensual por tipo de día
demanda_tipo <- data %>%  group_by(AÑO,MES,Tipo) %>% 
    summarise(demanda_mensual=sum(DEMANDA_SIN), num_dias=n()) 
View(demanda_tipo)

#creamos demanda_mes donde se encuentra la demanda mensual total para cada mes sin desagregar por tipo de día
demanda_mes <- data %>% group_by(AÑO,MES) %>% 
    summarise(demanda_mensual=sum(DEMANDA_SIN))
View(demanda_mes)
#Se crea la demanda mensual como serie de tiempo y se hace la descomposición aditiva de la demanda mensual
demanda_mes<- ts(demanda_mes[,3], start=c(2000,1),frequency = 12)

#Creamos Proporción donde se tiene la proporción de la demanda por tipo de día de forma mensual
Proporción<-demanda_tipo%>%mutate(mestot=sum(demanda_mensual))%>%group_by(MES,Tipo)%>%mutate(propdia=demanda_mensual/mestot)
View(Proporción)

###############################################################################################
# Cambiamos la demanda mes a logaritmos 
demanda_log <-log(demanda_mes)
demanda_log
#graficamos
plot(demanda_log)

#BuScamos cuál es el mejor modelo arima para prónosticos (forecast)
modelo_arimalog <- auto.arima(demanda_log)

#imprimimos el modelo para obtener más parametros de nuestro (Best model)
print(modelo_arimalog)

#Ahora vemos los residuos, ACF de los residuos y P- values de Ljung- Box 
tsdiag(modelo_arimalog)

#Probamos si existe ruido blanco
Box.test(residuals(modelo_arimalog), type = "Ljung-Box")

#probamos si tiene media=0
error=residuals(modelo_arimalog)
plot(error)

#Ahora se observan los residuos del modelo
checkresiduals(modelo_arimalog)

#Por lo tanto, ya podemos calcular el pronóstico de la demanda de energía para el proximo mes.

pronóstico_demandaSIN <- forecast(modelo_arimalog, h=6, level = c (95))
autoplot(pronóstico_demandaSIN)+
    ggtitle("Pronóstico Demanda de Energía SIN próximo mes") +
    ylab("GWh")
#Para observar los valores
print(summary(pronóstico_demandaSIN))

proyecciones <- as.data.frame(pronóstico_demandaSIN)
proyecciones
#Cambiamos a númerico 
names(proyecciones)[1] <-"Esc_Medio"
names(proyecciones)[2] <-"Lím_bajo"
names(proyecciones)[3] <-"Lím_alto"

proyecciones$Esc_Medio <- as.numeric(proyecciones$Esc_Medio)
proyecciones$Lím_bajo <- as.numeric(proyecciones$Lím_bajo)
proyecciones$Lím_alto <- as.numeric(proyecciones$Lím_alto)

#graficamos el escenario medio_point forecast
proyecciones <-exp(proyecciones)
ggplot(proyecciones)+geom_ribbon(aes(1:6,ymin=Lím_bajo,ymax=Lím_alto),fill="lightblue")+ geom_line(aes(1:6,Esc_Medio),color="red")+
ylab("GWh")

#################Modelo Arima Por tipo de día ################
####se crea una matrix con las dummies ORDINARIO y SÁBADO
TIPODIA <-model.matrix(~as.factor(Proporción$Tipo))
TIPODIA<-TIPODIA[,-1]
colnames(TIPODIA) <- c("ORDINARIO", "SÁBADO")
# se observa la tabla 
kable(head(TIPODIA, n=10))

#Creo una dummy para el año 2020

Proporción$dummy2020 = ifelse(Proporción$AÑO == 2020,1,0)

#Se crean las variables del modelo lineal ( variables exogenas)
variables_xreg<-cbind(Tipo=TIPODIA,Año2020=Proporción$dummy2020)

#Se crea la serie de tiempo
demanda <- ts(Proporción$demanda_mensual, frequency=36)
    
#### hacemos el modelo box.jenkins con la función auto.arima ###
Mod_arima <- auto.arima(demanda, xreg=variables_xreg)
summary(Mod_arima)

#Ahora vemos los residuos, ACF de los residuos y P- values de Ljung- Box 
tsdiag(Mod_arima)

#Probamos si existe ruido blanco
Box.test(residuals(Mod_arima), type = "Ljung-Box")

#probamos si tiene media=0
error=residuals(Mod_arima)
plot(error)

#Ahora se observan los residuos del modelo
checkresiduals(Mod_arima)

### hacer el pronóstico### un tipo de día ORDINARIO
variables.prediccion<-data.frame(1,0,0)
predi<-forecast(Mod_arima, xreg=as.matrix(variables.prediccion), level=c(95) )
predi

### hacemos el pronóstico####
ORDINARIO<-c(1,0,0)
SABADO<-c(0,1,0)
FESTIVO<-c(0,0,0)

variables_prediccion<-data.frame(rbind(ORDINARIO,SABADO,FESTIVO))

#cambiamos el nombre de las variables
names(variables_prediccion)[1] <-"ordinario"
names(variables_prediccion)[2] <-"sabado"
names(variables_prediccion)[3] <-"festivo"
variables_prediccion

fcast <- forecast(Mod_arima, xreg=as.matrix(variables_prediccion),level = c(95))
fcast
plot(fcast)
