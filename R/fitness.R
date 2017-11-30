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
#' 
#' #' Title
#' #'
#' #' @param poblacion list. Lista con los genotipos que componen la población.
#' #'
#' #' @return 
#' #' @export
#' #'
#' #' @examples
#' fitness <- function(poblacion){
#'   lapply(poblacion, fitness_sudoku)
#'   }