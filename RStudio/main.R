# Descomentar la siguiente linea para instalar los paquetes.
#install.packages(c("sp", "here"))

## Cargar librerias a utilizar
library(sp)
library(here)

## Raiz donde se encuentran los archivos del proyecto.
## Cambiar por la ruta que necesiten
root <- paste(here(), "/Proyectos/TDS115-Sim-Covid19/RStudio/", sep = "") 
setwd(root)
## Parametros (Casos iniciales por Depto, No de Dias a simular. 
## Poblacion total, Parametros de SIR)

## Simulacion (SIR Por Departamentos)

## Probabilidades de contagio (Por Departamentos)

## Mapa de El Salvador con Division Departamental
esa <- readRDS(paste(root, "gadm36_SLV_1_sp.rds", sep = ""))

## Mapa de El Salvador con Division Municipal
#esa2 <- readRDS(paste(root, "gadm36_SLV_2_sp.rds", sep = ""))

## Plotear Mapa de El Salvador
plot(esa)

## Recuperar datos de Confirmados por Departamento
confirmados <- read.csv(paste(root, "casos_municipios.csv", sep = ""), encoding = "UTF-8")
#confirmados
## Asignar la informacion de confirmados al mapa:
esa@data<-confirmados

#spplot(esa, col.regions = rainbow(16,  alpha = 0.75, rev = FALSE), main = "SARS Covid19", sub="El Salvador")
spplot(esa, col.regions = heat.colors(16,  alpha = 0.75, rev = TRUE), main = "SARS Covid19", sub="El Salvador")

