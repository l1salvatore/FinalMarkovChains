
if (!require(markovchain)) {
  install.packages('markovchain')
}

library(markovchain)
library(expm)

#Defino la matriz de transicion
transition_matrix_Ej1<- matrix(c(0.8, 0.15, 0.04, 0.01, 0,
                                 0.15, 0.8, 0.05, 0, 0,
                                 0.04, 0.12, 0.8, 0.04, 0,
                                 0.01, 0.04, 0.13, 0.8, 0.02,
                                 0,0,0,0.2,0.8), nrow = 5, byrow = TRUE)

#Instancion una nueva Markov Chain
markov_chain_Ej1<- new('markovchain', 
                     name = 'Ejercicio1', 
                     states = c("Desfavorecido", "MedioBajo", "Intermedio", "MedioAlto", "Alto"), 
                     transitionMatrix = transition_matrix_Ej1)


#Imprimo el resumen de la Markov Chain
summary(markov_chain_Ej1)

# Ejercicio del item c
#Defino el vector inicialv
v <- c(0.05, 0.15, 0.69, 0.1, 0.01)
#Calculo la alcanzabilidad en dos pasos. Esto es, v por el cuadrado de la matriz de transicion
v_2 <- v %*% (markov_chain_Ej1@transitionMatrix %^% 2)
#Imprimo
v_2

# Ejercicio del item d
#Imprimo la distribución estacionaria a largo plazo de la cadena
steadyStates(markov_chain_Ej1)

# Ejercicio del item e
# Calculo la potencia cuarta y me fijo la prob de alcanzabilidad de 3 a y
(markov_chain_Ej1@transitionMatrix %^% 4)[3,5]
