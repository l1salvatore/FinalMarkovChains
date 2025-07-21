
if (!require(markovchain)) {
  install.packages('markovchain')
}

library(markovchain)

transition_matrix_Ej1<- matrix(c(0.8, 0.15, 0.04, 0.01, 0,
                                 0.15, 0.8, 0.05, 0, 0,
                                 0.04, 0.12, 0.8, 0.04, 0,
                                 0.01, 0.04, 0.13, 0.8, 0.02,
                                 0,0,0,0.2,0.8), nrow = 5, byrow = TRUE)

markov_chain_Ej1<- new('markovchain', 
                     name = 'Ejercicio1', 
                     states = c("Desfavorecido", "MedioBajo", "Intermedio", "MedioAlto", "Alto"), 
                     transitionMatrix = transition_matrix_Ej1)


summary(markov_chain_Ej1)
