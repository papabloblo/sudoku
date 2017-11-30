generacion_poblacion <- function(valores_posibles,
                                 num_genes,
                                 tam_poblacion){
  replicate(n = tam_poblacion,
            random_integer_representation(valores_posibles, num_genes),
            simplify = FALSE
  )
}


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

  genes_fijos_NA <- is.na(genes_fijos)

  fitness <- lapply(lapply(poblacion_inicial,
                                      function(x){
                                        genes_fijos2 <- genes_fijos
                                        genes_fijos2[genes_fijos_NA] <- x
                                        return(genes_fijos2)
                                      }
                           ),
                    funcion_fitness
                    )


  traza <- list(iteracion = 0L,
                fitness = min(unlist(fitness)),
                tiempo = 0L)

  poblacion <- poblacion_inicial
  i <- 1L

  ini <- Sys.time()
  while (!any(fitness == 0) && i <= max_iter){

    mejor <- which(unlist(fitness) == min(unlist(fitness)))
    if (length(mejor) > 1){
      # Se muestrea para el caso en el que haya empate.
      mejor <- sample(mejor,1)  
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

    fitness <- lapply(lapply(poblacion,
                             function(x){
                               genes_fijos2 <- genes_fijos
                               genes_fijos2[genes_fijos_NA] <- x
                               return(genes_fijos2)
                             }
                             ),
                      funcion_fitness
                      )

    peor <- which(unlist(fitness) == max(unlist(fitness)))
    if (length(peor) > 1){
      # Se muestrea para el caso en el que haya empate.
      peor <- sample(peor,1)  
    }
    
    poblacion[[peor]] <- individuo_elitismo
    fitness[[peor]] <- fitness_elitismo

    if (!(i %% print_each)){
      cat("Iteración: ", i, "| Mejor valor de fitness: ", min(unlist(fitness)), "| Tiempo transcurrido: ", print(Sys.time() - ini))
    }

    traza$iteracion <- c(traza$iteracion, i)
    traza$fitness <- c(traza$fitness, min(unlist(fitness)))
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

