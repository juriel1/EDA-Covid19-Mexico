library(vroom)
library(readr)
library(anytime)

setwd("E:/_MyDocuments/Data/EDA-Covid19-Mexico")

### Objective: To find or not and visualize the relationship between the state origin and its symptoms
# Objetivo: Encontrar o no y visualizar la relación entre el origen estatal y su sintomatología

### The data was obtained directly from the "Mexico Open Data" page, so its cleaning and transformation has already
### been done, only the following changes were made to facilitate what was desired
# Los datos fueron obtenidos directamente de la página de "Datos abiertos México" por lo que su limpieza y
# transformación ya está hecha solo se hicieron los siguientes cambios para facilitar lo deseado

### Remove attributes: 'NACIONALIDAD','HABLA_LENGUA_INDIG','INDIGENA','TOMA_MUESTRA_LAB','RESULTADO_LAB', ‘ORIGEN',
### 'MIGRANTE','PAIS_NACIONALIDAD','PAIS_ORIGEN','UCI','ID_REGISTRO','CLASIFICACION_FINAL' considered unnecessary for
### the objective
# Se eliminar los atributos: 'NACIONALIDAD','HABLA_LENGUA_INDIG','INDIGENA','TOMA_MUESTRA_LAB','RESULTADO_LAB',
#‘ORIGEN','MIGRANTE','PAIS_NACIONALIDAD','PAIS_ORIGEN','UCI','ID_REGISTRO','CLASIFICACION_FINAL' al considerarse
#innecesarios para el objetivo

### The data was reduced to a third of the original size randomly, only considering the data with a multiple index
###of 3 in each year to facilitate its management with less computational expense.
# Se redujeron los datos a un tercio del tamaño original de manera aleatoria sólo considerando los datos con un índice
#múltiplo de 3 en cada año para facilitar su manejo con un menor gasto computacional 

new_csv <- function(i) {
  n_necessary <- c('HABLA_LENGUA_INDIG','INDIGENA',
                   'ORIGEN','MIGRANTE','UCI','ID_REGISTRO','CLASIFICACION_FINAL',
                   'FECHA_SINTOMAS','EMBARAZO')
  
  name        <- paste0('COVID19MEXICO202',as.character(i),'.csv')
  new_name    <- paste0('COVID19MEXICO202',as.character(i),'_PART.csv')
  date_def_ok <- as.Date("9999-12-27")

  dfo         <- read_csv(name)
  dfo         <- dfo[, -which(names(dfo) %in% n_necessary)]
  seq         <- seq(0, nrow(dfo), by = 3) 
  df          <- dfo[seq, , drop = FALSE]
  rm(dfo)
  gc()
  df$FECHA_DEF[is.na(df$FECHA_DEF)] <- date_def_ok
  rm(name)
  rm(n_necessary)
  rm(date_def_ok)
  rm(seq)
  gc()
  write_csv(df,new_name)
  rm(df)
  rm(new_name)
}

### On a machine with little computational power, the function must be applied to each file individually, never
### in a single run.
# En una máquina con poca potencia computacional se debe de hacer aplicar la función en cada archivo de manera
# individual nunca en una sola corrida

for (i in 0:3){
  new_csv(i)
}
rm(i)
#-------------------------------------------------------- OR
######new_csv(0)
#####new_csv(1)
#####new_csv(2)
#####new_csv(3)

### After analyzing the remaining data and objectifying the objectives that should be achieved so that they were
### more coherent together, it was decided to eliminate the following unnecessary attributes
# Después de analizar los datos restantes y objetivar los objetivos que se debían lograr de manera fueran más
# coherentes todos en conjunto se decidió eliminar los siguientes atributos innecesarios
  
### In addition, the column DEF and ENTIDAD_RES_N were added to indicate the death of the subject and the name
### of his state of origin.
# ademas se agrego la columna DEF y ENTIDAD_RES_N para indicar la defuncion del sujeto el nombre de su estado de
# origen

change_fields <- function(i){
  name        <- paste0('COVID19MEXICO202',as.character(i),'_PART.csv')
  date_def_ok <- as.Date("9999-12-27")
  df          <- read_csv(name)
  df$DEF      <- ifelse(df$FECHA_DEF == date_def_ok, 0, 1)
  write_csv(df,name)
  rm(name)
  rm(date_def_ok)
  rm(df)
}

for (i in 0:3){
  change_fields(i)
}
rm(i)
#-------------------------------------------------------- OR
#####change_fields(0)
#####change_fields(1)
#####change_fields(2)
#####change_fields(3)

limit_years <- function(i){
  name        <- paste0('COVID19MEXICO202',as.character(i),'_PART.csv')
  max_year    <- as.integer(120)
  df          <- read_csv(name)
  df$EDAD     <- ifelse(df$EDAD > 120, 120,df$EDAD)
  df$EDAD     <- ifelse(df$EDAD < 1, 1,df$EDAD)
  rm(name)
  rm(max_year)
  rm(df)
}

for (i in 0:3){
  limit_years(i)
}
rm(i)
#-------------------------------------------------------- OR
#####change_fields(0)
#####change_fields(1)
#####change_fields(2)
#####change_fields(3)
