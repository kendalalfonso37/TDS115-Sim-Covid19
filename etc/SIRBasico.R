library(deSolve)
#tamaño poblacional
N = 25000
#estado inicial de los compartimentos
init <- c(S = 1-1e-6,
          I = 1e-6,
          R = 0)
#parámetros del modelo (coeficientes de las variables)
# param <- c(beta = 1.4247,
#            gamma = 0.14286)
param <- c(beta = 1.1897,
           gamma = 0.4761)
#crear la función con las ODE
sir <- function(times, init, param) {
  with(as.list(c(init, param)), {
    #ecuaciones diferenciales   
    dS <- -beta * S * I
    dI <-  beta * S * I - gamma * I
    dR <-                 gamma * I
    #resultados de las tasas de cambio    
    return(list(c(dS, dI, dR)))
  })
}
#intervalo de tiempo y resolución
times <- seq(0, 50, by = 1)
#resolver el sistema de ecuaciones con función 'ode'
out <- ode(y = init, times = times, func = sir, parms = param)
#cambiar out a un data.frame
out <- as.data.frame(out*N) #aqui puede multiplicar 'out' por N
#eliminar la variable 'time' en out
out$time <- NULL
#gráfica
matplot(x = times, y = out, type = "l",
        xlab = "Tiempo", ylab = "S, I, R", main = "Modelo SIR Basico",
        lwd = 1, lty = 1, bty = "l", col = 2:4)
#a?adir leyendas de l?neas
legend(x = 30, y = 1e4, c("Susceptibles", "Infectados", "Recuperados"), 
       pch = 1, col = 2:4, bty = "n", cex = 1)
