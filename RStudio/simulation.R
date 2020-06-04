## Libreria para Hacer integracion de la EDO
#install.packages("deSolve")
library(deSolve)

# Librerias para dibujar mapa de calor y apuntar a root
#install.packages(c("sp", "here"))

## Cargar librerias a utilizar
library(sp)
library(here)

## Parametros a solicitar:
#tamaño poblacional
N = 25000

# Dias a Simular:
days <- 100

# Semilla de aleatoriedad:
seed <- 9
set.seed(seed)

#estado inicial de la simulacion (Tentativamente parametrizable)
init <- c(S = N - 1,
          I = 1,
          R = 0,
          m = 0)

#parámetros del modelo (coeficientes de las variables tentativamente constantes)
param <- c(beta = 0.24,
           gamma = 1./14,
           mu = 0.006,
           N = N)

#crear la función con las ODE
sir <- function(times, init, param) {
  with(as.list(c(init, param)), {
    #ecuaciones diferenciales   
    dS <- -beta * S * I / N
    dI <- beta * S * I / N - gamma * I - mu * I
    dR <- gamma * I
    dm <- mu * I
    #resultados de las tasas de cambio    
    return(list(c(dS, dI, dR, dm)))
  })
}

# intervalo de tiempo y resolución
times <- seq(0, days, by = 1)

# resolver el sistema de ecuaciones con función 'ode'
out <- ode(y = init, times = times, func = sir, parms = param)

# cambiar out a un data.frame
out <- as.data.frame(out) #aqui puede multiplicar 'out' por N

## Avisar sobre datos de la simulacion ejecutada con el Modelo SIR:
message("Datos Tabulados despues de ejecutar el modelo SIR: S - Susceptibles, I - Infectados, R - Recuperados, m - Fallecidos")

## (Se deja digits = 0 para que no muestre valores decimales la tabla out, luego se restablece a digits = 7)
options(digits = 0)

## Mostrar datos del modelo SIR Ejecutado
View(out)
options(digits = 7)

## Pausa de Ejecucion:
invisible(readline(prompt="Presione [enter] para continuar "))

# eliminar la variable 'time' en out
out$time <- NULL

# gráfica de Tendencias de SIRF
matplot(x = times, y = out, type = "l",
        xlab = "Tiempo (días)", ylab = "S, I, R, F (personas)", main = "Modelo SIR con Fallecidos (Tendencia)",
        lwd = 1, lty = 1, bty = "l", col = c(1,2,3,4))

# Leyendas de graficas
legend(x = 2, y = max(out$R), c("Susceptibles", "Infectados", "Recuperados", "Fallecidos"), 
       pch = 1, col = c(1,2,3,4), bty = "n", cex = 1)

## Avisar sobre Grafica de la Simulacion:
message("Grafica del Modelo SIR con Fallecidos:")

## Pausa de Ejecucion:
invisible(readline(prompt="Presione [enter] para continuar "))

# Obteniendo datos departamentales de contagios para mostrar en tabla:
susceptibles <- cbind(c(
  as.integer(runif(1, min = 0.04, max = 0.06)* out$S[nrow(out)]), # Ahuachapan   (0.05)
  as.integer(runif(1, min = 0.02, max = 0.03)* out$S[nrow(out)]), # Cabañas      (0.02)
  as.integer(runif(1, min = 0.02, max = 0.04)* out$S[nrow(out)]), # Chalatenango (0.03)
  as.integer(runif(1, min = 0.03, max = 0.05)* out$S[nrow(out)]), # Cuscatlan    (0.04)
  as.integer(runif(1, min = 0.10, max = 0.12)* out$S[nrow(out)]), # La Libertad  (0.11)
  as.integer(runif(1, min = 0.04, max = 0.06)* out$S[nrow(out)]), # La Paz       (0.05)
  as.integer(runif(1, min = 0.02, max = 0.04)* out$S[nrow(out)]), # La Union     (0.04)
  as.integer(runif(1, min = 0.06, max = 0.08)* out$S[nrow(out)]), # Morazán      (0.03)
  as.integer(runif(1, min = 0.04, max = 0.06)* out$S[nrow(out)]), # San Miguel   (0.07)
  as.integer(runif(1, min = 0.26, max = 0.28)* out$S[nrow(out)]), # San Salvador (0.27)
  as.integer(runif(1, min = 0.01, max = 0.03)* out$S[nrow(out)]), # San Vicente  (0.02)
  as.integer(runif(1, min = 0.08, max = 0.10)* out$S[nrow(out)]), # Santa Ana    (0.09)
  as.integer(runif(1, min = 0.06, max = 0.08)* out$S[nrow(out)]), # Sonsonate    (0.07)
  as.integer(runif(1, min = 0.04, max = 0.06)* out$S[nrow(out)])  # Usulutan     (0.05)
))

infectados <- cbind(c(
  as.integer(runif(1, min = 0.04, max = 0.06)* out$I[nrow(out)]), # Ahuachapan   (0.05)
  as.integer(runif(1, min = 0.02, max = 0.03)* out$I[nrow(out)]), # Cabañas      (0.02)
  as.integer(runif(1, min = 0.02, max = 0.04)* out$I[nrow(out)]), # Chalatenango (0.03)
  as.integer(runif(1, min = 0.03, max = 0.05)* out$I[nrow(out)]), # Cuscatlan    (0.04)
  as.integer(runif(1, min = 0.10, max = 0.12)* out$I[nrow(out)]), # La Libertad  (0.11)
  as.integer(runif(1, min = 0.04, max = 0.06)* out$I[nrow(out)]), # La Paz       (0.05)
  as.integer(runif(1, min = 0.02, max = 0.04)* out$I[nrow(out)]), # La Union     (0.04)
  as.integer(runif(1, min = 0.06, max = 0.08)* out$I[nrow(out)]), # Morazán      (0.03)
  as.integer(runif(1, min = 0.04, max = 0.06)* out$I[nrow(out)]), # San Miguel   (0.07)
  as.integer(runif(1, min = 0.26, max = 0.28)* out$I[nrow(out)]), # San Salvador (0.27)
  as.integer(runif(1, min = 0.01, max = 0.03)* out$I[nrow(out)]), # San Vicente  (0.02)
  as.integer(runif(1, min = 0.08, max = 0.10)* out$I[nrow(out)]), # Santa Ana    (0.09)
  as.integer(runif(1, min = 0.06, max = 0.08)* out$I[nrow(out)]), # Sonsonate    (0.07)
  as.integer(runif(1, min = 0.04, max = 0.06)* out$I[nrow(out)])  # Usulutan     (0.05)
))

recuperados <- cbind(c(
  as.integer(runif(1, min = 0.04, max = 0.06)* out$R[nrow(out)]), # Ahuachapan   (0.05)
  as.integer(runif(1, min = 0.02, max = 0.03)* out$R[nrow(out)]), # Cabañas      (0.02)
  as.integer(runif(1, min = 0.02, max = 0.04)* out$R[nrow(out)]), # Chalatenango (0.03)
  as.integer(runif(1, min = 0.03, max = 0.05)* out$R[nrow(out)]), # Cuscatlan    (0.04)
  as.integer(runif(1, min = 0.10, max = 0.12)* out$R[nrow(out)]), # La Libertad  (0.11)
  as.integer(runif(1, min = 0.04, max = 0.06)* out$R[nrow(out)]), # La Paz       (0.05)
  as.integer(runif(1, min = 0.02, max = 0.04)* out$R[nrow(out)]), # La Union     (0.04)
  as.integer(runif(1, min = 0.06, max = 0.08)* out$R[nrow(out)]), # Morazán      (0.03)
  as.integer(runif(1, min = 0.04, max = 0.06)* out$R[nrow(out)]), # San Miguel   (0.07)
  as.integer(runif(1, min = 0.26, max = 0.28)* out$R[nrow(out)]), # San Salvador (0.27)
  as.integer(runif(1, min = 0.01, max = 0.03)* out$R[nrow(out)]), # San Vicente  (0.02)
  as.integer(runif(1, min = 0.08, max = 0.10)* out$R[nrow(out)]), # Santa Ana    (0.09)
  as.integer(runif(1, min = 0.06, max = 0.08)* out$R[nrow(out)]), # Sonsonate    (0.07)
  as.integer(runif(1, min = 0.04, max = 0.06)* out$R[nrow(out)])  # Usulutan     (0.05)
))

fallecidos <- cbind(c(
  as.integer(runif(1, min = 0.04, max = 0.06)* out$m[nrow(out)]), # Ahuachapan   (0.05)
  as.integer(runif(1, min = 0.02, max = 0.03)* out$m[nrow(out)]), # Cabañas      (0.02)
  as.integer(runif(1, min = 0.02, max = 0.04)* out$m[nrow(out)]), # Chalatenango (0.03)
  as.integer(runif(1, min = 0.03, max = 0.05)* out$m[nrow(out)]), # Cuscatlan    (0.04)
  as.integer(runif(1, min = 0.10, max = 0.12)* out$m[nrow(out)]), # La Libertad  (0.11)
  as.integer(runif(1, min = 0.04, max = 0.06)* out$m[nrow(out)]), # La Paz       (0.05)
  as.integer(runif(1, min = 0.02, max = 0.04)* out$m[nrow(out)]), # La Union     (0.04)
  as.integer(runif(1, min = 0.06, max = 0.08)* out$m[nrow(out)]), # Morazán      (0.03)
  as.integer(runif(1, min = 0.04, max = 0.06)* out$m[nrow(out)]), # San Miguel   (0.07)
  as.integer(runif(1, min = 0.26, max = 0.28)* out$m[nrow(out)]), # San Salvador (0.27)
  as.integer(runif(1, min = 0.01, max = 0.03)* out$m[nrow(out)]), # San Vicente  (0.02)
  as.integer(runif(1, min = 0.08, max = 0.10)* out$m[nrow(out)]), # Santa Ana    (0.09)
  as.integer(runif(1, min = 0.06, max = 0.08)* out$m[nrow(out)]), # Sonsonate    (0.07)
  as.integer(runif(1, min = 0.04, max = 0.06)* out$m[nrow(out)])  # Usulutan     (0.05)
))

## Crear el data.frame a mostrar con los resultados:
tabulacion <- data.frame(susceptibles, infectados, recuperados, fallecidos, 
                         row.names = c("Ahuachapan", "Cabañas", "Chalatenango", "Cuscatlan",
                                       "La Libertad", "La Paz", "La Union", "Morazan", 
                                       "San Miguel", "San Salvador", "San Vicente",
                                       "Santa Ana", "Sonsonate", "Usulutan"))

# Avisar Tabulacion de datos por Departamento:
message("Tabulacion de datos de SIRF por Departamento en El Salvador en el día ", days,":")

# Mostrar Tabulacion
View(tabulacion)

## Pausa de Ejecucion:
invisible(readline(prompt="Presione [enter] para continuar "))

## Tabulacion de datos Globales:
total_susceptibles <- as.integer(out$S[nrow(out)])
total_infectados <- as.integer(out$I[nrow(out)])
total_recuperados <- as.integer(out$R[nrow(out)])
total_fallecidos <- as.integer(out$m[nrow(out)])

## Crear data frame con datos globales
tabGlobal <- data.frame(total_susceptibles, total_infectados, total_recuperados, total_fallecidos, 
                        row.names = c("Total"))

# Avisar Tabulacion de datos por Departamento:
message("Total de Susceptibles, Infectados, Recuperados y Fallecidos en El Salvador en el día ", days,":")

## Mostrar el data frame de datos globales:
View(tabGlobal)

## Pausa de Ejecucion:
invisible(readline(prompt="Presione [enter] para continuar "))


## Obtener el maximo de infectados y en que dia fue el pico de infectados:
max_infectados <- max(as.integer(out$I))
dia_max_infectados <- min(which(as.integer(out$I) == max_infectados))

## Crear Data Frame con los maximos de infectados, recuperados y fallecidos en la simulacion:
tabMax <- data.frame(max_infectados, dia_max_infectados)

# Avisar Tabulacion de datos por Departamento:
message("Maximo de Susceptibles, Infectados, Recuperados y Fallecidos en El Salvador:")

## Mostrar el data frame de datos globales:
View(tabMax)

## Pausa de Ejecucion:
invisible(readline(prompt="Presione [enter] para continuar "))


#### Generacion de Mapa de Calor:
## Raiz donde se encuentran los archivos del proyecto.
root <- here()
setwd(root)

## Escribir en csv los casos correspondientes:
write.csv(susceptibles, "susceptibles.csv")
write.csv(infectados, "infectados.csv")
write.csv(recuperados, "recuperados.csv")
write.csv(fallecidos, "fallecidos.csv")
write.csv(tabulacion, "tabulacion.csv")

## Mapa de El Salvador con Division Departamental
esa <- readRDS(paste(root, "/gadm36_SLV_1_sp.rds", sep = ""))

## Mapa de El Salvador con Division Municipal
#esa2 <- readRDS(paste(root, "gadm36_SLV_2_sp.rds", sep = ""))

## Plotear Mapa de El Salvador
# plot(esa)

## Aviso Generacion de Mapas de Calor.
message("Generacion de Mapas de Calor por Estado...")

##### Susceptibles
## Recuperar datos de Confirmados por Departamento
confirmados <- read.csv(paste(root, "/susceptibles.csv", sep = ""), encoding = "UTF-8")

## Asignar la informacion de confirmados al mapa:
confirmados$X <- NULL
esa@data <- confirmados

## Aviso de Mapa 
message("Mapa de Susceptibles ")

## Dibujar los casos de contagios en el mapa:
#spplot(esa, col.regions = rainbow(16,  alpha = 0.75, rev = FALSE), main = "Covid19 Susceptibles por Departamento", sub="El Salvador")
spplot(esa, col.regions = heat.colors(16,  alpha = 0.75, rev = TRUE), main = "Covid19 Susceptibles por Departamento", sub="El Salvador")

## Pausa de Ejecucion:
invisible(readline(prompt="Presione [enter] para continuar "))

##### Infectados
## Recuperar datos de Confirmados por Departamento
confirmados <- read.csv(paste(root, "/infectados.csv", sep = ""), encoding = "UTF-8")

## Asignar la informacion de confirmados al mapa:
confirmados$X <- NULL
esa@data <- confirmados

## Aviso de Mapa 
message("Mapa de Infectados ")

## Dibujar los casos de contagios en el mapa:
#spplot(esa, col.regions = rainbow(16,  alpha = 0.75, rev = FALSE), main = "Covid19 Infectados por Departamento", sub="El Salvador")
spplot(esa, col.regions = heat.colors(16,  alpha = 0.75, rev = TRUE), main = "Covid19 Infectados por Departamento", sub="El Salvador")

## Pausa de Ejecucion:
invisible(readline(prompt="Presione [enter] para continuar "))

##### Recuperados
## Recuperar datos de Confirmados por Departamento
confirmados <- read.csv(paste(root, "/recuperados.csv", sep = ""), encoding = "UTF-8")

## Asignar la informacion de confirmados al mapa:
confirmados$X <- NULL
esa@data <- confirmados

## Aviso de Mapa 
message("Mapa de Recuperados ")

## Dibujar los casos de contagios en el mapa:
#spplot(esa, col.regions = rainbow(16,  alpha = 0.75, rev = FALSE), main = "Covid19 Recuperados por Departamento", sub="El Salvador")
spplot(esa, col.regions = heat.colors(16,  alpha = 0.75, rev = TRUE), main = "Covid19 Recuperados por Departamento", sub="El Salvador")

## Pausa de Ejecucion:
invisible(readline(prompt="Presione [enter] para continuar "))

##### Fallecidos
## Recuperar datos de Confirmados por Departamento
confirmados <- read.csv(paste(root, "/fallecidos.csv", sep = ""), encoding = "UTF-8")

## Asignar la informacion de confirmados al mapa:
confirmados$X <- NULL
esa@data <- confirmados

## Aviso de Mapa 
message("Mapa de Fallecidos ")

## Dibujar los casos de contagios en el mapa:
#spplot(esa, col.regions = rainbow(16,  alpha = 0.75, rev = FALSE), main = "Covid19 Fallecidos por Departamento", sub="El Salvador")
spplot(esa, col.regions = heat.colors(16,  alpha = 0.75, rev = TRUE), main = "Covid19 Fallecidos por Departamento", sub="El Salvador")

## Pausa de Ejecucion:
invisible(readline(prompt="Presione [enter] para continuar "))

confirmados <- read.csv(paste(root, "/tabulacion.csv", sep = ""), encoding = "UTF-8")
confirmados$X <- NULL
esa@data <- confirmados
spplot(esa, col.regions = heat.colors(16,  alpha = 0.75, rev = TRUE), main = "Covid19 Casos por Departamento", sub="El Salvador")
