rm(list = ls())
# Resultados
library(tidyverse)
# load("data/sudoku_facil.RData")

resultados <- resultados_facil
rm(resultados_facil)
gc()

# Preparación de resultados -----------------------------------------------
str(resultados)
length(resultados)
length(resultados[[1]])
names(resultados[[1]][[1]])

# Iteración de ejemplo ----------------------------------------------------

iter <- resultados_dificil[[1]][[2]]

do.call(rbind, iter$fitness)

sapply(iter$fitness, min)

which.max(sapply(iter$fitness, length))


fitness_min <- sapply(iter$fitness, min)
fitness_max <- sapply(iter$fitness, max)
fitness_mean <- sapply(iter$fitness, mean)

resultados <- data_frame(iter = iter$iteracion,
                         min = fitness_min,
                         mean = fitness_mean,
                         max = fitness_max)

resultados %>%
  ggplot(aes(x = iter)) +
  geom_ribbon(aes(ymin = min,
                  ymax = max))

resultados2 <- data_frame(iter = iter$iteracion,
                         min = fitness_min,
                         mean = fitness_mean,
                         max = fitness_max)

resultados$id <- 1
resultados2$id <- 2

res <- bind_rows(resultados,
                 resultados2)

res %>%
  # group_by(id) %>%
  ggplot(aes(x = iter,
             y = min)) +
  geom_line(aes(group = id)) +
  ylim(0, NA)


iter <- resultados_dificil[[1]]

length(iter)

for (i in seq_along(iter)){
  iter[[i]]$id <- i
}

iter2 <- lapply(iter,
       function(x) data_frame(id = x$id,
                              iter = x$iteracion,
                              min = sapply(x$fitness, min))
       )



iter2 <- bind_rows(iter2)

iter2 %>%
  ggplot(aes(x = iter,
             y = min)) +
  geom_line(aes(group = id)) +
  ylim(0, NA)


a <- function(z){
  for (i in seq_along(z)){
    z[[i]]$id <- i
  }

  x2 <- lapply(z,
                  function(x){
                    aa <- sapply(x$fitness, min)
                    data_frame(id = x$id,
                               iter = x$iteracion,
                               min = aa,
                               tam_poblacion = x$parametro$tam_poblacion,

                               prob_cruce = x$parametro$prob_cruce,
                               tam_torneo = x$parametro$tam_torneo,
                               prob_mutacion = x$parametro$prob_mutacion)
                  }
  )

  x2 <- bind_rows(x2)

}

b <- lapply(resultados, a)
z <- resultados_dificil[[1]]
z[[1]]$iteracion

aa <- sapply(x$fitness, min)
data_frame(id = x$id,
           x = x$iteracion,
           min = aa)

for (i in seq_along(b)){
  b[[i]]$id2 <- i
}

b <- bind_rows(b)

library(ggthemes)
b %>%
  ggplot(aes(x = iter,
             y = min)) +
  geom_line(aes(group = paste(id, id2))) +
  ylim(0, NA) +
  geom_smooth(color = "firebrick") +
  theme_fivethirtyeight()



mejores_iter <- b %>%
  group_by(id, id2) %>%
  top_n(-1, wt = min) %>%
  top_n(-1, wt = iter)

mejores_iter %>%
  ggplot(aes(x = prob_mutacion,
             y = min)) +
  geom_point(color = "steelblue",
             alpha = 0.5,
             size = 5) +
  geom_smooth(color = "firebrick", method = "lm") +
  ylim(0, NA) +
  theme_fivethirtyeight()
summary(lm(formula = min ~ prob_mutacion, data = mejores_iter))

mejores_iter %>%
  ggplot(aes(x = prob_mutacion,
             y = iter)) +
  geom_point(color = "steelblue",
             alpha = 0.5,
             size = 5) +
  geom_smooth(color = "firebrick", method = "lm") +
  ylim(0, NA) +
  theme_fivethirtyeight()
summary(lm(formula = iter ~ prob_mutacion, data = mejores_iter))


mejores_iter <- mejores_iter %>%
  mutate(min = min - mean())
