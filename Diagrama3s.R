library(EvolutionaryGames)
A <- matrix(c(1, 2, 0, 0, 1, 2, 2, 0, 1), 3, byrow=T)
state <- matrix(c(0.7, 0.2, 0.1), 1, 3, byrow=TRUE)

phaseDiagram3S(A, Replicator, NULL, state, FALSE, TRUE, strategies = c("Tolerant","Resistant", "Insect"))

