library(ggplot2)

ProcesoPois <- function(t, lambda){
  N <- rpois(1, t * lambda)
  C <- sort(runif(N, 0, t))
  data.frame(x = c(0, C), y = 0:N)
}

# Datos simulados
N6 <- ProcesoPois(0.5, 18)

# Construcción de segmentos horizontales
segmentos <- data.frame(
  x_inicio = N6$x,
  x_fin = c(N6$x[-1], 0.5),  # finaliza en 0.5
  y = N6$y
)

# Gráfico con solo líneas horizontales y puntos
ggplot() +
  geom_segment(data = segmentos, aes(x = x_inicio, xend = x_fin, y = y, yend = y),
               color = "red", size = 1) +
  geom_point(data = N6, aes(x = x, y = y), color = "red", size = 2) +
  scale_x_continuous(name = "Tiempo (años)", breaks = round(unique(N6$x), 1)) +
  scale_y_continuous(name = "N(t)", breaks = 0:max(N6$y)) +
  ggtitle("Núcleos familiares que llegan para los próximos 6 meses") +
  theme_minimal() +
  theme(
    # Etiquetas de los ejes (los números)
    axis.text.x = element_text(face = "bold", color = "black", size = 10),
    axis.text.y = element_text(face = "bold", color = "black", size = 10)
  )


# Datos simulados para 15 años
N15 <- ProcesoPois(15, 18)

# Tiempos entre llegadas
interarrivals <- diff(N15$x)

summary(interarrivals)

# Histograma con densidad
hist(interarrivals, breaks = 20, probability = TRUE, col = "skyblue",
     main = "Histograma de T (tiempos entre llegadas en años)",
     xlab = "Tiempo entre llegadas (Años)")

lines(density(interarrivals), col = "darkblue", lwd = 2)
curve(dexp(x, rate = 18), add = TRUE, col = "red", lwd = 2, lty = 2)
legend("topright", legend = c("Densidad empírica", "Exponencial teórica"), 
       col = c("darkblue", "red"), lty = c(1, 2), lwd = 2)



n <- 100000
medianas <- replicate(n, mean(diff(ProcesoPois(15,18)$x)))
summary(medianas)

