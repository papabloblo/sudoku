#' Random integer representation
#'
#' @param valores vector. Valores posibles que puede tener cada gen.
#' @param tam integer. Tamaño del genotipo.
#'
#' @return
#' @export
#'
#' @examples
random_integer_representation <- function(valores, tam){
  sample(valores, size = tam, replace = TRUE)
}

#' Torneo
#'
#' @param poblacion list. Lista con los genotipos de la población.
#' @param fitness_poblacion vector. Valor de fitness para cada genotipo de la población.
#' @param k integer. Número de competidores en el torneo.
#'
#' @return 
#' @export
#'
#' @examples
torneo <- function(poblacion, 
                   fitness_poblacion, 
                   k){
  competidores <- sample(seq_along(poblacion), k)
  mejor_competidor <- competidores[which.min(fitness_poblacion[competidores])]
  return(poblacion[[mejor_competidor]])
}

seleccion_padres <- function(num_padres,
                             poblacion, 
                             fitness_poblacion,
                             k = 10){
  replicate(n = num_padres,
            torneo(
              poblacion = poblacion, 
              fitness_poblacion = fitness_poblacion, 
              k = k
            ),
            simplify = FALSE
  )
}


one_point_crossover <- function(padres, prob_cruce){
  # Supongo que todos los padres tienen la misma longitud
  # Solo preparado para 2 PADRES
  punto <- sample(seq_along(padres[[1]]), 1)
  
  hijos <- list()
  if (runif(1) < prob_cruce && punto < length(padres[[1]])){
    
    hijos[[1]] <- c(padres[[1]][1:punto],
                    padres[[2]][(punto + 1):length(padres[[1]])])
    
    hijos[[2]] <- c(padres[[2]][1:punto],
                    padres[[1]][(punto + 1):length(padres[[1]])])
    
  } else {
    
    hijos[[1]] <- padres[[1]]
    hijos[[2]] <- padres[[2]]
    
  }
  return(hijos) 
}

nuevos_hijos <- function(x, 
                         x_fitness,
                         num_padres, 
                         num_emparejamientos = length(x)/num_padres,
                         prob_cruce, 
                         k){
  
  # Falta el caso en el que el tamaño de la población no sea divisible por num_padres.
  
  emparejamientos <- replicate(n = num_emparejamientos, 
                               seleccion_padres(num_padres, 
                                                poblacion = x,
                                                fitness_poblacion = x_fitness,
                                                k), 
                               simplify = FALSE)
  return(
    unlist(
      lapply(emparejamientos,
             one_point_crossover,
             prob_cruce = prob_cruce
      ),
      recursive = FALSE
    )
  )
}

# Mutación ----------------------------------------------------------------

random_resetting <- function(x, prob, valores_posibles){
  genes_mutantes <- runif(length(x)) < prob
  x[genes_mutantes] <- sample(valores_posibles,
                              size = sum(genes_mutantes),
                              replace = TRUE)
  return(x)
}

mutacion_poblacion <- function(x, prob_mutacion, valores_posibles){
  return(
    lapply(x, 
           FUN = random_resetting, 
           prob = prob_mutacion,
           valores_posibles = valores_posibles
    )
  )
}

