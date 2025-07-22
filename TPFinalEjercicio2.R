options(scipen = 999)
# Cantidad de simulaciones
n <- 400000
# Con rbinom generamos 350 'viviendas' con probabilidad 0.37 de tener certificado (1) o de no tener(0)
# Esas 350 las sumamos para obtener un conteo. Replicamos n veces para obtener un vector conteos
# Y en base a ese vector o lista, realizamos el gráfico correspondiente
conteos <- replicate(n, sum(rbinom(350, 1, 0.37)))

# Obtener la tabla de frecuencias
frecuencias <- table(conteos)
# Convertir a vectores para graficar con líneas
x_vals <- as.numeric(names(frecuencias))
y_vals <- as.numeric(frecuencias)
# Graficar como curva
plot(x_vals, y_vals, type = "o", pch = 19, col = "darkgreen",
     main =  paste("Conteo de número de éxitos en ", n, "repeticiones de rbinom(350, 1, 0.37)"),
     xlab = "Cantidad de éxitos (1s)", ylab = "Frecuencia (repeticiones)")

summary(conteos)
