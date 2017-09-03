# Para ver los tiempos de ejecucion
tiempos <- function(x) {
  t_total <- function(y) {
    y$times$everything[3]
  }
  n_iter <- function(z) {
    nrow(z$results)
  }
  a <<- unlist(lapply(x, t_total))
  names(a) <- names(x)
  b <<- unlist(lapply(x, n_iter))
  cat("Tiempo Total : \n")
  print(a[order(a)])
  print("Numero Iteracciones : \n")
  print(b[order(a)])
  print("Tiempo por Iteracción : \n")
  b <- a/b
  print(round(b[order(a)],2))
}

plots <- function(lista_modelos) {
  
  procesa <- function(modelo) {
    
    cat("METODO : ", modelo$method, " (", modelo$modelInfo$label, ")", "\n", sep ="")
    cat("Best Tune :\n")
    print(modelo$bestTune)
    cat("\nRMSE: ", round(min(modelo$results$RMSE),4), "\n")
    cat("R2: ", round(min(modelo$results$Rsquared),4), "\n")
    cat("\nTiempo Total (seg.) : ", round(modelo$times$everything[3],2), "\n")
    cat("Numero Iteracciones : ", nrow(modelo$results), "\n")
    cat("Tiempo por Iteraccion : ", 
        round((modelo$times$everything[3] / nrow(modelo$results)),2), "\n\n")
    
    if (nrow(modelo$results) > 1) {
      print(plot(modelo, 
                 main = paste(modelo$method, " : ", modelo$modelInfo$label)))
    } else {
      cat("Sin parametros: NO PLOT\n")
    }
    invisible(readline(prompt="Press [enter] to continue"))
  }
  
  invisible(lapply(lista_modelos, procesa))
  
}