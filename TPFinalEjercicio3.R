library(ggplot2)

# Definición de la función ProcesoPois(t, lambda)
# Genero un número N con rpois
# Genero un vector de N números desde 0 a t y los ordeno
# Retorno el data.frame con los resultados
ProcesoPois <- function(t, lambda){
  N <- rpois(1, t * lambda)
  C <- sort(runif(N, 0, t))
  data.frame(x = c(0, C), y = 0:N)
}

# Simulación de una trayectoria para los próximos 6 meses
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


# Simulación de una trayectoria para los próximos 15 meses
N15 <- ProcesoPois(15, 18)

#Guardo en interarrivals las diferencias entre los tiempos de llegadas (x)
interarrivals <- diff(N15$x)

# Imprimo el análisis numérico
summary(interarrivals)

# Histograma con densidad
hist(interarrivals, breaks = 20, probability = TRUE, col = "skyblue",
     main = "Histograma de T (tiempos entre llegadas en años)",
     xlab = "Tiempo entre llegadas (Años)")

curve(dexp(x, rate = 18), add = TRUE, col = "red", lwd = 2, lty = 2)


# Replico n veces el proceso de Poisson y calculo su media
n <- 100000
medias <- replicate(n, mean(diff(ProcesoPois(15,18)$x)))
summary(medias)

