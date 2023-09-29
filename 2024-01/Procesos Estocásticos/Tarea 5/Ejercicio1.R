library(ggplot2)
# Dada una matriz de transición y una distribución inicial
# se simulan los estados X0,...,Xn de una cadena de Markov.
MCsimulate <- function(P, pi_inicial){
  X <- c()
  # elegimos el estado inicial
  x0 <- sample(colnames(P), 1, prob = pi_inicial)
  X <- append(X, x0)
  n = 2
  while(X[n-1] != "$0" && X[n-1] != "$8K" ){
    # La proba de X_n | X_n-1
    p <- P[X[n-1], ]
    xn <- sample(colnames(P), 1, prob = p)
    X <- append(X, xn)
    n = n + 1
  }
  return(X)
}

tau <- function(X){
  return(length(X)-1)
}


# ---- Primer escenario: apuesta sólo de mil en mil ------------ #

# definición de las probabilidades de transición
p <- function(i, j){
  if(i >= 1 & i<= 7){
    if(j == i+1){
      return(0.4)
    }else if(j == i-1){
      return(0.6)
    }else{
      return(0)
    }
  }else{
    if(j == i){
      return(1)
    }else{return(0)}
    }
}
# definición de la matriz de transición P.
# alocación de memoria.
P <- matrix(data = rep(0, 9*9), 
            nrow = 9, ncol =9)
# llenado de la matriz.
for(i in 1:nrow(P)){
  for(j in 1:ncol(P)){
    P[i,j] = p(i-1, j-1)
  }
}
# cambio de nombres de los estados.
rownames(P) <- colnames(P) <- c("$0", "$1K", "$2k", "$3k", "$4K",
                                "$5K", "$6K", "$7K", "$8K")
# probabilidad inicial.
pi_inicial <- c(0,0,0,1,0,0,0,0,0)
# número de simulaciones.
N = 100000
# Cálculo del número de éxitos, donde definimos como
# éxito si el preso logra conseguir los 8,000 pesos.
Y <- replicate(N, "$8K" %in% MCsimulate(P, pi_inicial))
# Obtención del intervalo de confianza de p.
prop.test(sum(Y), N, conf.level = 0.95)
# Obtención del primer tiempo de absorción
T <- replicate(N, tau(MCsimulate(P, pi_inicial)))
# distribución de tau
ggplot(aes(x=T), data = NULL) + 
  geom_histogram(binwidth = 4, fill = "lightblue") +
  labs(x = "Primer tiempo de absorción", y = "Frecuencia\n") + 
  ggtitle("Distribución del primer tiempo de absorción",
          subtitle = "para apuesta de mil en mil pesos")
# media muestral de T
mean(T)
# -------- Segundo escenario: apuesta total ------------ #

# definición de las probabilidades de transición
p <- function(i, j){
  if(i >= 1 & i<= 4){
    if(j == 2*i){
      return(0.4)
    }else if(j == 0){
      return(0.6)
    }else{
      return(0)
    }
  }else if(i>=5 & i<=7){
    if(j == 8){
      return(0.4)
    }else if(j == 2*i-8){
      return(0.6)
    }else{
      return(0)
    }
  }
  else{
    if(j == i){
      return(1)
    }else{return(0)}
  }
}
# definición de la matriz de transición P.
# alocación de memoria.
P <- matrix(data = rep(0, 9*9), 
            nrow = 9, ncol =9)
# llenado de la matriz.
for(i in 1:nrow(P)){
  for(j in 1:ncol(P)){
    P[i,j] = p(i-1, j-1)
  }
}

rownames(P) <- colnames(P) <- c("$0", "$1K", "$2k", "$3k", "$4K",
                                "$5K", "$6K", "$7K", "$8K")

N = 100000
Y <- replicate(N, "$8K" %in% MCsimulate(P, pi_inicial))
# Obtención del intervalo de confianza de p.
prop.test(sum(Y), N, conf.level = 0.95)
# Obtención del primer tiempo de absorción
T <- replicate(N, tau(MCsimulate(P, pi_inicial)))
# distribución de tau
ggplot(aes(x=T), data = NULL) + 
  geom_histogram(binwidth = 1, fill = "lightblue") +
  labs(x = "Primer tiempo de absorción", y = "Frecuencia\n") + 
  ggtitle("Distribución del primer tiempo de absorción",
          subtitle = "para apuesta total")
# media muestral de T
mean(T)
