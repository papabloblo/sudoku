# Datos de entrada --------------------------------------------------------
source("sudoku_prueba.R")

# Determinación de parámetros ---------------------------------------------

# Random Search -----------------------------------------------------------

set.seed(1234)
n <- 100

tam_poblacion <- sample(10:200, size = n, replace = TRUE)
prob_cruce <- runif(n = n, min = 0.5, max = 1)
tam_torneo <- sample(2:10, size = n, replace =TRUE)
prob_mutacion <- runif(n = n, min = 0.001, max = 0.2)
# prob_mutacion <- runif(n = n, min = 0.001, max = 0.2)

grid <- data.frame(tam_poblacion,
                   prob_cruce,
                   tam_torneo,
                   prob_mutacion
                   )

# Ejecución de pruebas ----------------------------------------------------

library(parallel)

system.time(resultados_dificil <- parallel::mcmapply(pruebas_ga,
                                       num_pruebas              = 10,


                                       num_padres               = 2,

                                       tam_poblacion            = grid$tam_poblacion,
                                       prob_cruce               = grid$prob_cruce,
                                       tam_torneo               = grid$tam_torneo,
                                       prob_mutacion            = grid$prob_mutacion,

                                       max_iter                 = 5000,
                                       print_each               = 100,


                                       MoreArgs = list(generacion_poblacion_ini = generacion_poblacion,
                                                       genes_fijos              = sudoku_dificil,
                                                       funcion_fitness          = fitness_sudoku,

                                                       valores_posibles         = 1:9,
                                                       valores_mutacion         = 1:9
                                                      )
                                       ,
                                       SIMPLIFY = FALSE
                                       ))

generar_data_frame <- function(resultado){
  resultado <- lapply(resultado, function(x){
    do.call(cbind, x)
  })


  resultado <- lapply(seq_along(resultado),
               function(x){
                 resultado[[x]]$id <- x
                 resultado[[x]]
               }
  )

  return(do.call(rbind, resultado))
}

resultados_dificil <- do.call(rbind,
                            lapply(resultados_dificil,
                                   generar_data_frame))

save(resultados_dificil, file = "data/sudoku_dificil.RData")

library(tidyverse)
library(ggthemes)
resultados_facil <- tbl_df(resultados_facil)

resultados_facil <- resultados_facil %>%
  mutate(clave = paste0(parametros.tam_poblacion,
                        parametros.prob_cruce,
                        parametros.prob_mutacion,
                        parametros.tam_torneo),
         clave2 = paste0(clave, id))

library(tidyverse)
library(ggthemes)

# Última iteración
ultim_iter <- resultados_facil %>%
  group_by(clave2) %>%
  top_n(-1, wt = fitness) %>%
  top_n(-1, wt = iteracion) %>%
  ungroup()

analisis_prob_mut <- ultim_iter %>%
  group_by(clave,
           parametros.prob_mutacion) %>%
  summarise(fit_media  = mean(fitness),
            fit_sd     = sd(fitness),
            iter_media = mean(iteracion),
            iter_sd    =  sd(iteracion)) %>%
  arrange(fit_media)


analisis_prob_mut %>%
  ggplot(aes(x = parametros.prob_mutacion,
             y = fit_media)) +
    geom_point(aes(size = iter_media)) +
    geom_smooth()

analisis_prob_mut %>%
  ggplot(aes(x = parametros.prob_mutacion,
             y = fit_media)) +
  geom_point(aes(size = fit_sd),
             alpha = 0.7,
             color = "steelblue") +
  geom_smooth(colour = "firebrick",
              method = "loess",
              alpha = 0,
              size = 2) +
  theme_fivethirtyeight() +
  ggtitle("Probabilidad de mutación vs. valor de fitness")



analisis_prob_mut %>%
  ggplot(aes(x = parametros.prob_mutacion,
             y = iter_media)) +
  geom_point(aes(size = iter_sd),
             alpha = 0.7,
             color = "steelblue") +
  geom_smooth(colour = "firebrick",
              method = "loess",
              alpha = 0,
              size = 2) +
  theme_fivethirtyeight() +
  ggtitle("Probabilidad de mutación vs. iteraciones")




analisis_prob_mut %>%
  ggplot(aes(x = iter_media)) +
  geom_density(
             alpha = 0.7,
             color = "steelblue") +
  theme_fivethirtyeight() +
  ggtitle("Probabilidad de mutación vs. iteraciones")


analisis_prob_mut %>%
  ggplot(aes(x = iter_media,
             group = cut_number(parametros.prob_mutacion, 2),
             fill = cut_number(parametros.prob_mutacion, 2))) +
  geom_density(
    alpha = 0.7,
    color = "steelblue") +
  theme_fivethirtyeight() +
  ggtitle("Probabilidad de mutación vs. iteraciones")



analisis_prob_mut %>%
  ggplot(aes(x = parametros.prob_mutacion,
             y = fit_media,
             group = cut_number(parametros.prob_mutacion, 30)))+
geom_violin(scale = "width") +
  geom_point()

ultim_iter %>%
  ggplot(aes(x = factor(parametros.prob_mutacion),
             y = fitness)) +
  geom_boxplot(alpha = 0) +
  theme_fivethirtyeight()



analisis_prob_mut %>%
  ggplot() +
  geom_point(aes(x = parametros.prob_mutacion,
                 y = iter_media))

resultados_facil2 <- resultados_facil[1:55000,]
resultados_facil %>%
  ggplot(aes(x = iteracion,
             y = fitness)) +
  geom_line(aes(group = clave2),
            alpha = 0.5) +
  geom_smooth(size = 2, color = "firebrick") +
  theme_fivethirtyeight()


resultados_facil %>%
  ggplot(aes(x = iteracion,
             y = fitness)) +
  # geom_line(aes(group = clave2),
  #           alpha = 0.5) +
  geom_smooth(aes(group = clave),
              alpha = 0.1,
              color = "black",
              size = 0.5) +
  ylim(c(0, 75)) +
  theme_fivethirtyeight()




# Análisis de resultados --------------------------------------------------

res <- resultados_facil %>%
  filter(iteracion == 5000) %>%
  group_by(parametros.tam_poblacion,
           parametros.prob_cruce,
           parametros.prob_mutacion,
           parametros.tam_torneo) %>%
  summarise(fitness_media = mean(fitness)) %>%
  ungroup() %>%
  arrange(fitness_media)


res <- resultados_facil %>%
  filter(iteracion == 5000) %>%
  group_by(clave) %>%
  summarise(fitness_media = mean(fitness)) %>%
  ungroup() %>%
  arrange(fitness_media) %>%
  top_n(n = -20, wt = fitness_media)

res %>%
  left_join(resultados_facil) %>%
  ggplot(aes(x = iteracion,
             y = fitness)) +
  # geom_line(aes(group = clave2),
  #           alpha = 0.5) +
  geom_smooth(aes(group = clave),
              alpha = 0.1,
              color = "black",
              size = 0.5) +
  ylim(c(0, 75)) +
  theme_fivethirtyeight()

mejores_20 <- res %>%
  left_join(resultados_facil)


mejores_20 %>%
  ggplot(aes(x = iteracion,
             y = fitness)) +
  geom_line(aes(group = clave2),
            alpha = 0.25)
  ylim(c(0, 75)) +
  theme_fivethirtyeight()


mejores_20 %>%
  ggplot(aes(x = parametros.prob_mutacion,
             y = fitness)) +
  geom_jitter(height = 0) +
  geom_smooth()



mejores_20 %>%
  ggplot(aes(x = factor(parametros.prob_mutacion),
             y = fitness)) +
  geom_violin()
