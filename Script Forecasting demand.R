#Primeramente es necesario descargar las siguientes librerias:
install.packages("tseries")
install.packages("forecast")
install.packages("lubridate")
install.packages("astsa")
install.packages("tidyverse")
install.packages("foreign")
install.packages("quantmod")

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


#Subir base de datos de XM demanda de energía SIN (GWh)
data <- read_excel("~/Desktop/TESIS MAYE/data.xlsx")
Festivos_ <- read_excel("~/Desktop/TESIS MAYE/Festivos .xlsx") # Subir base de datos festivos en Colombia

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
Proporción<-demandatipo%>%mutate(mestot=sum(demanda_mensual))%>%group_by(MES,Tipo)%>%mutate(propdia=demanda_mensual/mestot)
View(Proporción)

###############################################################################################

#Al contar con nuestra serie de tiempo demanda_mes debemos analizar si es estacionaria.

#Ahora graficamos la descomposición de la serie 
plot(decompose(demanda_mes))

#Otra forma de graficar la descomposición
descom=decompose(demanda_mes)
autoplot(descom)

#Ahora revisamos la función de autocorrelación (ACF)
acf(demanda_mes)

#Luego revisamos la función de autocorrelación parcial (PACF)
pacf(demanda_mes)

#Se realiza la prueba de dickey fuller para la serie demanda_mes
adf.test(demanda_mes, alternative="stationary")

#creamosla serie estacionaria usando logaritmos o por diferencias
#primeramente analizamos por log
demandalog=log(demanda_mes)
demandalog
plot(demandalog)
#se realiza la prueba de dickey fuller
adf.test(demandalog, alternative="stationary")

#Luego hacemos la serie con diferencias 
demanda_dif <- diff(demanda_mes)
demanda_dif
autoplot(demanda_dif)
plot(demanda_dif)
# Nuevamente se realiza la prueba de dickey fuller
adf.test(demanda_dif, alternative="stationary")
#como el p-value es menor a 0,05 la serie ahora es estacionaria
# ahora vemos el ACF Y PACF
acf(demanda_dif)
pacf(demanda_dif)

######  MODELO ARIMA ######

auto.arima(demanda_mes)
#Buacamos cual es el mejor modelo arima para prónosticos (forecast)
modelo_arima <- auto.arima(demanda_mes,d=1,D=1, stepwise =  FALSE, approximation =  FALSE, trace = TRUE)

#imprimimos el modelo para obtener más parametros de nuestro (Best model)
print(modelo_arima)

#Ahora vemos los residuos, ACF de los residuos y P- values de Ljung- Box 
tsdiag(modelo_arima)

#Probamos si existe ruido blanco
Box.test(residuals(modelo_arima), type = "Ljung-Box")

#probamos si tiene media=0
error=residuals(modelo_arima)
plot(error)

#Ahora se observan los residuos del modelo
checkresiduals(modelo_arima)

#Por lo tanto, ya podemos calcular el pronóstico de la demanda de energía para el proximo mes.

pronóstico_demandaSIN <- forecast(modelo_arima, h=1, level = c (95))
autoplot(pronóstico_demandaSIN)+
ggtitle("Pronóstico Demanda de Energía SIN próximo mes") +
ylab("GWh")
#Para observar los valores
print(summary(pronóstico_demandaSIN))


rm
