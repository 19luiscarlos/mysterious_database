setwd("~/Documents/Documentos/iop/TEC/Mysterious")
batches <- list()

for (i in 1:14) {
  file <- paste('batch', i, '.csv', sep = '')
  batches[[i]] <- read.csv(file = file)
}

# Diferencias respecto a la columna anterior, fila por fila

graficas <- function(grado = 0, columna = 1, color = 'verde') {
  for (i in 1:14) {
    n <- nrow(batches[[i]])
    dummy1 <- batches[[i]][1:(n-1), columna]
    dummy2 <- batches[[i]][2:n, columna]
    una_dif <- dummy2 - dummy1
    
    m <- length(dummy1)
    dummier1 <- una_dif[1:(m-1)]
    dummier2 <- una_dif[2:m]
    dos_dif <- dummier2 - dummier1
    
    if (grado == 0) {
      graf <- batches[[i]][, columna]
    }else if(grado == 1){
      graf <- una_dif
    }else if(grado == 2){
      graf <- dos_dif
    }
    
    if (columna == 1) {
      cols = c('#15ef32', '#14e22f', '#13d32c', '#11c429', '#10b526',
               '#0fa823', '#0f9e22', '#0f8e20', '#0e821d', '#0d771b',
               '#0c6b19', '#0b6016', '#0a5915', '#094f12', '#084210')
    }else if(columna == 2) {
      cols = c('#28fcd5', '#26f2cc', '#29e5c2', '#27dbba', '#25d1b1', 
               '#24c9ab', '#22bfa2', '#21b79b', '#1fad93', '#1da38a',
               '#1b9981', '#198e79', '#187f6c', '#177564', '#146657')
    }else if(columna == 3) {
      cols = c('#32c1ff', '#30b8f2', '#2daee5', '#2ba4d8', '#299cce', 
               '#2795c4', '#278fbc', '#2385af', '#217da5', '#1f789e',
               '#1d6e91', '#1a6484', '#175b77', '#14516b', '#11475e')
    }else if(columna == 4){
      cols = c('#ff77ab', '#f26da0', '#e56294', '#d35686', '#c64d7c', 
               '#bc4774', '#b2426d', '#a53b64', '#99345a', '#8e2c52',
               '#82294b', '#772544', '#6d1f3d', '#661b38', '#5e1531')
    }
    
    if (grado == 0) {
      
      if (columna == 1) {
        ylim = c(9, 152)
        offsets <- 0*c(0:15)
      }else if(columna == 2){
        ylim = c(0, 196)
        offsets <- 0*c(0:15)
      }else if(columna == 3){
        ylim = c(0,500)
        offsets <- 40*c(0:14)
      }else if(columna == 4){
        ylim = c(0,410)
        offsets <- 30*c(0:14)
      }
      
    }else if (grado == 1){
      
      if (columna == 1 || columna == 2) {
        ylim = c(0,67)
        offsets <- 5*c(0:14)
      }else if(columna == 3) {
        ylim = c(-17,610)
        offsets <- 50*c(0:14)
      }else if(columna == 4) {
        ylim = c(-10, 420)
        offsets <- 32*c(0:14)
      }
      
    }else if (grado == 2){
      
      if (columna == 1) {
        ylim = c(0, 90)
        offsets <- 7*c(0:14)
      }else if(columna == 2){
        ylim = c(0, 65)
        offsets <- 5*c(0:15)
      }else if(columna == 3) {
        ylim = c(-17, 850)
        offsets <- 64*c(0:14)
      }else if(columna == 4) {
        ylim = c(-10, 730)
        offsets <- 55*c(0:14)
      }
      
    }
    
    if (i==1) {
      plot(graf + offsets[i], type = 'l', col = cols[i], 
           ylim = ylim, xaxt = "n", yaxt = "n", xlab = '', ylab = '')
    }else{
      lines(graf + offsets[i], col = cols[i])
    }
  }
}

graficas(grado = 0, columna = 1)
graficas(grado = 0, columna = 2)
graficas(grado = 0, columna = 3)
graficas(grado = 0, columna = 4)

graficas(grado = 1, columna = 1)
graficas(grado = 1, columna = 2)
graficas(grado = 1, columna = 3)
graficas(grado = 1, columna = 4)

graficas(grado = 2, columna = 1)
graficas(grado = 2, columna = 2)
graficas(grado = 2, columna = 3)
graficas(grado = 2, columna = 4)
