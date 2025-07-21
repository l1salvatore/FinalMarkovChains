# rbinom -> procesos de Bernoulli
# rpois -> procesos de Poisson
# rmarkovchain -> procesos de Markov Chain

if (!require(markovchain)) {
  install.packages('markovchain')
}

library(markovchain)

transition_matrix_A <- matrix(c(1/2, 1/4, 1/4,
                                2/3, 0, 1/3,
                                3/5, 2/5, 0), nrow = 3, byrow = TRUE)

markov_chain_A<- new('markovchain', 
                      name = 'Ejercicio10A', 
                      states = c("a", "b", "c"), 
                      transitionMatrix = transition_matrix_A)


summary(markov_chain_A)



transition_matrix_B <- matrix(c(0.5, 0, 0, 0.5, 0,
                                0, 0.6, 0, 0, 0.4,
                                0.3, 0, 0.7, 0, 0,
                                0, 0, 1, 0 ,0,
                                0, 1, 0, 0, 0), nrow = 5, byrow = TRUE)

markov_chain_B <- new('markovchain', 
                    name = 'Ejercicio10B', 
                    states = c("a", "b", "c", "d", "e"), 
                    transitionMatrix = transition_matrix_B)


summary(markov_chain_B)

calc_prob_seq_A <- function(mc, seq) {
  probSequence <- 1
  for (i in 1 : (length(seq) -1 )){
    prob_transition <- mc@transitionMatrix[seq[i], seq[i+1]]
    probSequence <- probSequence * prob_transition
  }
  
  print(probSequence)
}

M12 <- matrix(c(1, 0, 0, 0, 0, 0, 0, 0,
                1/2, 0, 1/2, 0, 0, 0, 0, 0,
                0, 1/2, 0, 1/2, 0, 0, 0, 0,
                0, 0, 1/2, 0, 1/2, 0, 0, 0,
                0, 0, 0, 1/2, 0, 1/2, 0, 0,
                0, 0, 0, 0, 1/2, 0, 1/2,0,
                0, 0, 0, 0, 0, 1/2, 0, 1/2,
                0, 0, 0, 0, 0, 0, 0, 1), nrow = 8, byrow = TRUE)

markov_chain_12 <- new('markovchain', 
                      name = 'Ejercicio12', 
                      states = c("0", "1","2", "3", "4", "5", "6" ,"7"), 
                      transitionMatrix = M12)

simular_hasta_absorcion <- function(mc, estado_inicial) {
  camino <- estado_inicial
  actual <- estado_inicial
  while(!(actual %in% c("0", "7"))) {
    actual <- rmarkovchain(n=1, object=markov_chain_12, t0 = actual)
    camino <- c(camino, actual)
  }
  return(camino)
}

for(i0 in c("1", "3", "6")) {
  camino <- simular_hasta_absorcion(mc12, i0)
  print(camino)
  cat("Largo:", length(camino)-1, "pasos. Estado final:", tail(camino,1), "\n\n")
  
}