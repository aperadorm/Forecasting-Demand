#Subir base de datos de XM demanda SIN 
library(readxl)
data <- read_excel("~/Desktop/TESIS MAYE/data.xlsx")
View(data)
library(readxl)
DATA_TP <- read_excel("~/Desktop/TESIS MAYE/DATA TP.xlsx")
View(DATA_TP)
# Subir base de datos festivos en Colombia
library(readxl)
Festivos_ <- read_excel("~/Desktop/TESIS MAYE/Festivos .xlsx")
View(Festivos_)
#Cambiamos el formato a fecha 
data$fecha <- as.Date(paste(data$`Tiempo - Año` ,data$`Tiempo - Mes`,data$`Tiempo - Día`,sep = "-"),format = "%Y-%m-%d")
#Agregamos columna que nos muestre el día
data$dia <- weekdays(data$fecha) 
#Catalogamos los tipos de día, Lunes, martes, miercoles, jueves y viernes= ORDINARIO, sabado= SABADO, domingo y festivos= FESTIVOS

data[data$dia=="monday" | data$dia="tuesday" | data$dia="wednesday" | data$dia="thursday" | data$dia="friday" , ]$tipo <-"ORDINARIO"
data[data$dia=="saturday", ]$tipo <-"SABADO"
data[data$dia=="sunday",]$tipo <-"FESTIVO"
rm(Festivos_Colombia_2010_2021)
rm(Festivos)