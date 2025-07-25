options(scipen = 999)
# Cantidad de simulaciones

# Obtener la tabla de frecuencias para rbinom
frecuencias <- rbinom(400000, 350, 0.37)
frecuenciasTable <- table(frecuencias)

# Convertir a vectores para graficar con líneas
x_vals <- as.numeric(names(frecuenciasTable))
y_vals <- as.numeric(frecuenciasTable)
# Graficar como curva
plot(x_vals, y_vals, type = "o", pch = 19, col = "darkgreen",
     main =  paste("Número de ensayos de acuerdo al número de viviendas con certificado Re.Na.Ba.P"),
     xlab = "Cantidad de viviendas con certificado", ylab = "Frecuencia (número de ensayos")

summary(frecuencias)
