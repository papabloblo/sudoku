
# Representación de cada individuo ----------------------------------------

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

# Función de fitness de Sudoku --------------------------------------------

# Función genérica para comprobar que, dado un vector,
# cuántos elementos iguales hay de cada posición.
# Es decir, comprobar_i(x) = \sum_{j \ne i} I(x_i == x_j).
# Devuelve un vector con la cantidad de elementos iguales a esa posición
# hay en el vector original.
#' Comprobar
#'
#' @param x vector. Vector de genotipo
#'
#' @return
#' @export
#'
#' @examples 
comprobar <- function(x){
  s <- integer(length(x))
  for (elemento in seq_along(x)){
    s[elemento] <- sum(x == x[elemento]) - 1L
  }
  return(s)
}

comprobar_sub_cuadricula <- function(x, ind_cuadricula){
  fitness_cuad <- integer(length(x))
  for (id_cuad in unique(ind_cuadricula)){
    cuad <- x[ind_cuadricula == id_cuad]
    
    # Trabajarlo de forma teórica para acortar
    fitness_cuad[ind_cuadricula == id_cuad] <- comprobar(cuad)
  }
  return(fitness_cuad)
}

comprobar_fila <- function(x){
  return(t(apply(x, 1, comprobar)))
}

comprobar_columna <- function(x){
  return(apply(x, 2, comprobar))
}

ind_cuadricula <- c(
  rep(rep(x = 1:3, each = 3), 3), 
  rep(rep(x = 4:6, each = 3), 3),
  rep(rep(x = 7:9, each = 3), 3))

fitness_sudoku <- function(x, ind_cudricula){
  x <- matrix(x,
              ncol = 9,
              nrow = 9)

  columnas <- comprobar_columna(x)
  filas <- comprobar_fila(x)
  cuadricula <- comprobar_sub_cuadricula(x, ind_cuadricula)
  
  return(sum(columnas + filas + cuadricula)/2)
}


fitness_sudoku_old <- function(x){
  x <- matrix(x,
              ncol = 9,
              nrow = 9)
  
  ind_cuadricula <- c(
    rep(rep(x = 1:3, each = 3), 3), 
    rep(rep(x = 4:6, each = 3), 3),
    rep(rep(x = 7:9, each = 3), 3))
  
  columnas <- comprobar_columna(x)
  filas <- comprobar_fila(x)
  cuadricula <- comprobar_sub_cuadricula(x, ind_cuadricula)
  
  return(sum(columnas + filas + cuadricula)/2)
}


fitness <- function(x) lapply(x, fitness_sudoku)

# Selección de padres -----------------------------------------------------

torneo <- function(poblacion, fitness_poblacion, k){
  competidores <- sample(seq_along(poblacion), k)
  mejor_competidor <- competidores[which.min(fitness_poblacion[competidores])]
  return(poblacion[[mejor_competidor]])
}

padres <- function(num_padres, poblacion, fitness_poblacion, k = 10){
  replicate(n = num_padres,
            torneo(
              poblacion = poblacion, 
              fitness_poblacion = fitness_poblacion, 
              k  = k
            ),
          simplify = FALSE
        )
}

# Cruce -------------------------------------------------------------------
# Cruce por un punto (One-point crossover)

one_point_crossover <- function(x, prob_cruce){
  # Supongo que todos los padres tienen la misma longitud
  # Solo preparado para 2 PADRES
  punto <- sample(seq_along(x[[1]]), 1)
  
  hijos <- list()
  if (runif(1) < prob_cruce && punto < length(x[[1]])){
    
    hijos[[1]] <- c(x[[1]][1:punto],
                    x[[2]][(punto + 1):length(x[[1]])])
                    
    hijos[[2]] <- c(x[[2]][1:punto],
                    x[[1]][(punto + 1):length(x[[1]])])
    
  } else {
    
    hijos[[1]] <- x[[1]]
    hijos[[2]] <- x[[2]]
    
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
                               padres(num_padres, 
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


# Esbozo del algoritmo ----------------------------------------------------

algoritmo_genetico <- function(sudoku,
                               tam_poblacion = 10,
                               # semilla,
                               max_iter = 100,
                               print_each = 100,
                               num_padres = 2,
                               prob_cruce = 0.75,
                               tam_torneo = 2,
                               prob_mutacion = 0.01
                               ){
  
  
  # set.seed(semilla)

  sudoku_0 <- sudoku == 0  
  l <- sum(sudoku_0)  

  
  poblacion <- replicate(n = tam_poblacion, 
                             random_integer_representation(1:9, l),
                             simplify = FALSE
                             )  
  
  
  poblacion_para_fitness <- lapply(poblacion, 
                                   function(x){
                                     sudoku2 <- sudoku
                                     sudoku2[sudoku_0] <- x
                                     return(sudoku2)
                                     }
                                   )
  

  f <- fitness(poblacion_para_fitness)  
  
  
  traza <- list(iteracion = c(0L),
                f = min(unlist(f)),
                tiempo = 0L)
  
  ini <- Sys.time()
  i <- 1L
  while (!any(f == 0) && i <= max_iter){
    
    mejor <- sample(which(unlist(f) == min(unlist(f))), 1)
    individuo_elitismo <- poblacion[[mejor]]
    f_elitismo <- f[[which.min(unlist(f))]]
    
    # cat("Iteración: ", i)
    poblacion <- nuevos_hijos(poblacion,
                              f,
                              num_padres = num_padres,
                              prob_cruce = prob_cruce,
                              k = tam_torneo)
    
    poblacion <- mutacion_poblacion(poblacion,
                                    prob_mutacion = prob_mutacion,
                                    valores_posibles = 1:9
    )
    
    
    poblacion_para_fitness <- lapply(poblacion, function(x){
      sudoku2 <- sudoku
      sudoku2[sudoku_0] <- x
      return(sudoku2)
    })
    
    
    f <- fitness(poblacion_para_fitness)
    peor <- sample(which(unlist(f) == max(unlist(f))), 1)
    poblacion[[peor]] <- individuo_elitismo
    f[[peor]] <- f_elitismo
    
    if (!(i %% print_each)){
      cat("Iteración: ", i)
      print(min(unlist(f)))
      print(Sys.time() - ini)
    }
    
    traza$iteracion <- c(traza$iteracion, i)
    traza$f <- c(traza$f, min(unlist(f)))
    traza$tiempo <- c(traza$tiempo, Sys.time() - ini)
    
    i <- i + 1L
  }
  traza$parametros <- data.frame(tam_poblacion,
                                 max_iter,
                                 print_each,
                                 num_padres,
                                 prob_cruce,
                                 tam_torneo,
                                 prob_mutacion)
  return(traza)
  
}


pruebas_ga <- function(tam_poblacion,
                       
                       max_iter,
                       print_each,
                       num_padres,
                       
                       prob_cruce,
                       tam_torneo,
                       prob_mutacion,
                       
                       num_pruebas = 10){
  
  resultados <- replicate(num_pruebas,
                          algoritmo_genetico(sudoku         = sudoku_facil,
                                              tam_poblacion = tam_poblacion,
                                              
                                              max_iter      = max_iter,
                                              print_each    = print_each,
                                              num_padres    = num_padres,
                                              
                                              prob_cruce    = prob_cruce,
                                              tam_torneo    = tam_torneo,
                                              prob_mutacion = prob_mutacion
                                            ),
                         simplify = FALSE
                         )
  return(resultados)
}

# GRID --------------------------------------------------------------------

# n <- 2
tam_poblacion <- floor(seq(10, 100, length.out = 4))
prob_cruce <- seq(0.5, 1, length.out = 3)
tam_torneo <- floor(seq(3, 10, length.out = 3))
prob_mutacion <- seq(0.001, 0.2, length.out = 5)

grid <- expand.grid( prob_cruce = prob_cruce,
             tam_torneo = tam_torneo,
             prob_mutacion = prob_mutacion,
             tam_poblacion = tam_poblacion)

dim(grid)


# Random Search -----------------------------------------------------------
set.seed(1234)
n <- 100
tam_poblacion <- sample(10:200, size = n, replace = TRUE)
prob_cruce <- runif(n = n, min = 0.5, max = 1)
tam_torneo <- sample(2:10, size = n, replace =TRUE)
prob_mutacion <- runif(n = n, min = 0.001, max = 0.2)

grid <- data.frame(tam_poblacion,
                   prob_cruce,
                   tam_torneo,
                   prob_mutacion
                   )

library(parallel)

resultados <- mcmapply(pruebas_ga,
         
         tam_poblacion = grid$tam_poblacion,
         
         max_iter = 1000,
         print_each = 250,
         num_padres = 2,
         
         prob_cruce = grid$prob_cruce,
         tam_torneo = grid$tam_torneo,
         prob_mutacion = grid$prob_mutacion,
         # MoreArgs = list(sudoku = sudoku_facil),
         
         num_pruebas = 10,
         SIMPLIFY = FALSE)

a <- lapply(resultados,
       function(x){
         y <- lapply(x, data.frame)
         y <- lapply(1:length(y),
                     function(x){
                       y[[x]]$id <- x
                       return(y[[x]])
                     })
         do.call(rbind, y)
         
       })

resultados <- do.call(rbind, a)


save(resultados, file = paste0("Práctica 1/resultados_facil", Sys.Date(), ".RData"))
