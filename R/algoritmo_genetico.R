
#' Algoritmo genético
#'
#' @param poblacion_inicial list. Lista con los individuos de la población inicial.
#' @param funcion_fitness function.
#' @param genes_fijos vector.
#' @param valores_mutacion vector.
#' @param num_padres integer.
#' @param prob_cruce float.
#' @param tam_torneo integer.
#' @param prob_mutacion float.
#' @param tam_poblacion integer.
#' @param max_iter integer. Número máximo de iteraciones del algoritmo genético.
#' @param print_each integer. Número de iteraciones entre las que
#'  se muestra información por pantalla
#'
#' @return
#' @export
#'
#' @examples
algoritmo_genetico <- function(poblacion_inicial,

                               funcion_fitness,

                               genes_fijos,

                               valores_mutacion,

                               num_padres    = 2,
                               prob_cruce    = 0.75,
                               tam_torneo    = 2,
                               prob_mutacion = 0.01,
                               tam_poblacion = 10,

                               max_iter      = 100,
                               print_each    = 100
                               ){

  fitness <- lapply(poblacion_inicial,
                    funcion_fitness,
                    solucion_inicial = genes_fijos)


  traza <- list(iteracion = 1:max_iter,
                fitness = list(unlist(fitness)),
                tiempo = numeric(max_iter))

  poblacion <- poblacion_inicial
  i <- 2L

  ini <- Sys.time()
  while (!any(fitness == 0) && i <= max_iter){

    mejor <- which(unlist(fitness) == min(unlist(fitness)))
    if (length(mejor) > 1){
      # Se muestrea para el caso en el que haya empate.
      mejor <- sample(mejor, 1)  
    }
    

    # Se conserva el mejor individuo para usarlo en elitismo
    individuo_elitismo <- poblacion[[mejor]]
    fitness_elitismo <- fitness[[mejor]]

    poblacion <- nuevos_hijos(poblacion,
                              fitness,
                              num_padres = num_padres,
                              prob_cruce = prob_cruce,
                              k          = tam_torneo
                              )

    poblacion <- mutacion_poblacion(poblacion,
                                    prob_mutacion = prob_mutacion,
                                    valores_posibles = valores_mutacion
                                    )

    fitness <- lapply(poblacion,
                      funcion_fitness,
                      ind_cuadricula = ind_cuadricula,
                      solucion_inicial = genes_fijos)

    peor <- which(unlist(fitness) == max(unlist(fitness)))
    if (length(peor) > 1){
      # Se muestrea para el caso en el que haya empate.
      peor <- sample(peor, 1)  
    }
    
    poblacion[[peor]] <- individuo_elitismo
    fitness[[peor]] <- fitness_elitismo

    if (!(i %% print_each)){
      cat("Iteración: ", i, "| Mejor valor de fitness: ", min(unlist(fitness)), "| Tiempo transcurrido: ", print(Sys.time() - ini))
    }

    traza$fitness[[i]] <- unlist(fitness)
    traza$tiempo[i] <- Sys.time() - ini

    i <- i + 1L
  }
  traza$poblacion_final <- poblacion
  traza$parametros <- data.frame(tam_poblacion,
                                 max_iter,
                                 print_each,
                                 num_padres,
                                 prob_cruce,
                                 tam_torneo,
                                 prob_mutacion)
  return(traza)
}



pruebas_ga <- function(num_pruebas = 10,
                       generacion_poblacion_ini,
                       tam_poblacion,
                       valores_posibles,
                       funcion_fitness,

                       genes_fijos,

                       valores_mutacion,

                       num_padres,
                       prob_cruce,
                       tam_torneo,
                       prob_mutacion,

                       max_iter,
                       print_each){

  num_genes <- sum(is.na(genes_fijos))


  poblacion_inicial <- generacion_poblacion_ini(valores_posibles = valores_posibles,
                                            num_genes = num_genes,
                                            tam_poblacion = tam_poblacion)

  resultados <- parallel::mclapply(seq(num_pruebas),
                                   function(x)
                                     algoritmo_genetico(
                                       poblacion_inicial = poblacion_inicial,

                                       funcion_fitness,

                                       genes_fijos,

                                       valores_mutacion,

                                       num_padres,
                                       prob_cruce,
                                       tam_torneo,
                                       prob_mutacion,
                                       tam_poblacion = tam_poblacion,

                                       max_iter,
                                       print_each
                                       )
                                   )


  return(resultados)
}

