library(ggplot2)

ProcesoPois <- function(t, lambda){
  N <- rpois(1, t * lambda)
  C <- sort(runif(N, 0, t))
  data.frame(x = c(0, C), y = 0:N)
}

# Datos simulados
N6 <- ProcesoPois(6, 1.5)

# Construcción de segmentos horizontales
segmentos <- data.frame(
  x_inicio = N6$x,
  x_fin = c(N6$x[-1], 6),  # finaliza en 6
  y = N6$y
)

# Gráfico con solo líneas horizontales y puntos
ggplot() +
  geom_segment(data = segmentos, aes(x = x_inicio, xend = x_fin, y = y, yend = y),
               color = "red", size = 1) +
  geom_point(data = N6, aes(x = x, y = y), color = "red", size = 2) +
  scale_x_continuous(name = "Tiempo (meses)",   unique(round(N6$x, 1))) +
  scale_y_continuous(name = "N(t)", breaks = 0:max(N6$y)) +
  ggtitle("Proceso de Poisson para los próximos 6 meses") +
  theme_minimal()


# Datos simulados para 15 años
N15 <- ProcesoPois(180, 1.5)
# Tiempos entre llegadas
interarrivals <- diff(N15$x)

summary(interarrivals)

# Histograma con densidad
hist(interarrivals, breaks = 20, probability = TRUE, col = "skyblue",
     main = "Histograma de T (tiempos entre llegadas en meses)",
     xlab = "Tiempo entre llegadas (Meses)")

lines(density(interarrivals), col = "darkblue", lwd = 2)
curve(dexp(x, rate = 1.5), add = TRUE, col = "red", lwd = 2, lty = 2)
legend("topright", legend = c("Densidad empírica", "Exponencial teórica"), 
       col = c("darkblue", "red"), lty = c(1, 2), lwd = 2)



n <- 100000
medianas <- replicate(n, mean(diff(ProcesoPois(180,1.5)$x)))
summary(medianas)

