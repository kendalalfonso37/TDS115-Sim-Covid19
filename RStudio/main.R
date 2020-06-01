setwd("C:/Users/kenda/Proyectos/CovidSimulacion")
install.packages(c("sp", "here"))
library(sp)

## Parametros (Casos iniciales por Depto, No de Dias a simular. 
## Poblacion total, Parametros de SIR)

## Simulacion (SIR Por Departamentos)

## Probabilidades de contagio (Por Departamentos)

## Mapa de El Salvador con Division Departamental
esa <- readRDS("gadm36_SLV_1_sp.rds")

## Mapa de El Salvador con Division de Municipios
esa2 <- readRDS("gadm36_SLV_2_sp.rds")

plot(esa)

#ss <- esa[esa$NAME_1=='San Salvador']

#writeClipboard(unique(esa@data$NAME_1))

confirmados <- read.csv("casos_municipios.csv", encoding = "UTF-8")
confirmados
esa@data<-confirmados

#spplot(esa, col.regions = rainbow(16,  alpha = 0.75, rev = FALSE), main = "SARS Covid19", sub="El Salvador")
spplot(esa, col.regions = heat.colors(16,  alpha = 0.75, rev = TRUE), main = "SARS Covid19", sub="El Salvador")

