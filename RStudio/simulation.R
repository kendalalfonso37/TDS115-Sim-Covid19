## Libreria para Hacer integracion de la EDO
#install.packages("deSolve")
library(deSolve)

## Guardar opciones por defecto
#default_options = options()
#save(default_options, file = "default_options.rda")

## Deshabilitar Notacion Exponencial y Digitos Decimales
options(scipen = 0,  digits = 0)

#tamaño poblacional
N = 10000
# Dias a Simular:
days <- 100
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
#intervalo de tiempo y resolución
times <- seq(0, days, by = 1)
#resolver el sistema de ecuaciones con función 'ode'
out <- ode(y = init, times = times, func = sir, parms = param)
#cambiar out a un data.frame
out <- as.data.frame(out) #aqui puede multiplicar 'out' por N
#eliminar la variable 'time' en out
out$time <- NULL
#gráfica de Tendencias de SIRF
matplot(x = times, y = out, type = "l",
        xlab = "Tiempo (días)", ylab = "S, I, R, F (personas)", main = "Modelo SIR con Fallecidos (Tendencia)",
        lwd = 1, lty = 1, bty = "l", col = c(1,2,3,4))
#a?adir leyendas de l?neas
legend(x = 2, y = max(out$R), c("Susceptibles", "Infectados", "Recuperados", "Fallecidos"), 
       pch = 1, col = c(1,2,3,4), bty = "n", cex = 1)

## Obteniendo las cantidades diarias de Infectados, Recuperados y Fallecidos
# deltas <- data.frame(cumsum(out))
# #deltas <- abs(deltas)
# deltas$S <- NULL
# times <- 1:days
# 
# matplot(x = 1:nrow(deltas), y = deltas, type = "l",
#         xlab = "Tiempo (días)", ylab = "I, R, F (personas)", main = "Modelo SIR con Fallecidos (Incrementos y Decrementos)",
#         lwd = 1, lty = 1, bty = "l", col = c(2,3,4))
# 
# legend(x = 2, y = max(deltas$I), c("Infectados", "Recuperados", "Fallecidos"), 
#        pch = 1, col = c(2,3,4), bty = "n", cex = 1)

min(out$S)
max(out$I)
max(out$R)
max(out$m)

## Cargar de nuevo opciones por defecto:
load("default_options.rda")
options(default_options)
