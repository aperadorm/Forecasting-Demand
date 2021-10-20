#Subir base de datos de XM demanda SIN GWh
library(readxl)
data <- read_excel("~/Desktop/TESIS MAYE/data.xlsx")
Festivos_ <- read_excel("~/Desktop/TESIS MAYE/Festivos .xlsx") # Subir base de datos festivos en Colombia

#Cambiamos el formato a fecha 
data$fecha <- as.Date(paste(data$`Tiempo - Año` ,data$`Tiempo - Mes`,
                            data$`Tiempo - Día`,sep = "-"),format= "%Y-%m-%d")

#Agregamos columna que nos muestre el día
data$dia <- weekdays(data$fecha) 

#Unir base de datos data con Festivos_
library(dplyr)
names(Festivos_)[2] <- "tipo"
names(Festivos_)[1] <- "fecha"
data <- data %>% left_join(Festivos_, by="fecha")

#Catalogamos los tipos de día, Lunes a viernes= ORDINARIO, sabado= SABADO, domingo y festivos= FESTIVOS

names(Festivos_) <- c("fecha", "tipo")

data$tipo2[data["dia"] == "Monday" & is.na(data["tipo"]) == T] <- "ORDINARIO"
data$tipo2[data["dia"] == "Tuesday"& is.na(data["tipo"]) == T] <- "ORDINARIO"
data$tipo2[data["dia"] == "Wednesday"& is.na(data["tipo"]) == T] <- "ORDINARIO"
data$tipo2[data["dia"] == "Thursday"& is.na(data["tipo"]) == T] <- "ORDINARIO"
data$tipo2[data["dia"] == "Friday"& is.na(data["tipo"]) == T] <- "ORDINARIO"
data$tipo2[data["dia"] == "Saturday"& is.na(data["tipo"]) == T] <- "SÁBADO"
data$tipo2[data["dia"] == "Sunday"& is.na(data["tipo"]) == T] <- "FESTIVO"

data$tipo2[data["tipo"] == "FESTIVO"] <- "FESTIVO"
data <-data [,-7]
names(data)[1] <- "AÑO"
names(data)[2] <- "MES"
names(data)[3] <- "DIA"
names(data)[7] <- "Tipo"
names(data)[4] <-"DEMANDA_SIN"

#Calculamos la demanda mensual por tipo de día 
data2 <- data %>%  group_by(AÑO,MES,Tipo) %>% 
    summarise(demanda_mensual=sum(DEMANDA_SIN)) 

#Cambiamos AÑO Y MES de chr a num
data2$AÑO <- as.numeric(data2$AÑO)
data2$MES<- as.numeric(data2$MES)

data2 <-data2 %>% arrange(AÑO,MES)
View(data2)

#creamos serie donde se encuentra la demanda mensual total para cada año
serie <- data %>% group_by(AÑO,MES) %>% 
    summarise(demanda_mensual=sum(DEMANDA_SIN))

serie$AÑO <- as.numeric(serie$AÑO)
serie$MES<- as.numeric(serie$MES)

serie <-serie %>% arrange(AÑO,MES)
View(serie)

#Se realiza la descomposición aditiva de la demanda mensual
serie <- ts(serie[,3], start=c(2000,1),frequency = 12)
plot(decompose(serie))




