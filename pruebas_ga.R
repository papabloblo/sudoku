
# Ejecuciones del algoritmo -----------------------------------------------

library(ggplot2)
library(ggthemes)


a <- pruebas_ga(num_pruebas = 100,
           generacion_poblacion_ini = generacion_poblacion,
           tam_poblacion = 10,

           valores_posibles = 1:9,
  
  


            genes_fijos = sudoku_facil,
    
            
            
            funcion_fitness = fitness_sudoku,
            
            valores_mutacion = 1:9,
            
            num_padres    = 2,
            prob_cruce    = 0.75,
            tam_torneo    = 2,
            prob_mutacion = 0.01,
            
            max_iter      = 10000,
            print_each    = 100)

a <- lapply(a, function(x){
  do.call(cbind, x)
})


a2 <- lapply(seq_along(a),
            function(x){
              a[[x]]$id <- x
              a[[x]]
              } 
            )

a3 <- do.call(rbind, a2)


a3 %>% 
  ggplot(aes(x = iteracion,
             y = f)) + 
  geom_line(aes(group = id),
            alpha = 0.5) + 
  geom_smooth(size = 2, color = "firebrick", method = "loess") + 
  theme_fivethirtyeight()

library(tidyverse)

lapply(lapply(a,
       function(x){
         do.call(cbind, x)
         }
       ),
       function
         
       
       a2 <- lapply(a,
                   function(x){
                     y <- do.call(x, cbind)
                     y <- lapply(1:length(y),
                                 function(x){
                                   y[[x]]$id <- x
                                   return(y[[x]])
                                 })
                     do.call(rbind, y)
                     
                   })