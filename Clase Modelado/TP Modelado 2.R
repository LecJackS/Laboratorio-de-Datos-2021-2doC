## -------------------------------------------------------------------------------------
library(dplyr)
library(tidyverse) # Para usar drop_na()
library(ggplot2)
library(palmerpenguins)

head(penguins)


## -------------------------------------------------------------------------------------
datos <- penguins %>% 
    rowwise() %>% 
    mutate(species = toString(species)) %>% 
    filter(species == "Gentoo") %>% 
    select(bill_length_mm, body_mass_g) %>% 
    drop_na()

datos %>% 
    ggplot(aes(bill_length_mm, body_mass_g)) +
        geom_point()


## -------------------------------------------------------------------------------------
modelo.lineal <- function(x, ordenada=0, pendiente=1){
    y <- pendiente * x + ordenada
    return (y)
}


## -------------------------------------------------------------------------------------
#set.seed(42)

a <- runif(1, min=-1, max=1)
b <- runif(1, min=0, max=1)

xs <- c(0,1)

ys <- modelo.lineal(xs, b, a)


plot(xs, ys,
    xlim = c(0, 1),
    ylim = c(-2, 2),
    type = 'l',
    main = paste0("Función: ", round(a, 2), " x + ", round(b, 2))
    )
grid()



## -------------------------------------------------------------------------------------
root.mean.squared.error <- function(y.real, y.pred){
    e <- sum( (y.real - y.pred) ^ 2 )
    return (e)
}


## -------------------------------------------------------------------------------------
root.mean.squared.error(c(1, 2, 3), c(0, 0, 0))


## -------------------------------------------------------------------------------------
set.seed(42)
optim.aleatoria <- function(datos,
                            loss.fun=root.mean.squared.error,
                            pendiente.ini=0.01, ordenada.ini=1){
  facred <- 0.9999  # factor de reduccion de la ventana
  facred.acu <- 1   # factor de reduccion acumulado
  toler <- 1e-6     # umbral de tolerancia
  
  # Rangos
  rango.pendiente <- 10 # rango inicial de mu
  rango.ordenada <- 100 # rango inicial de alfa
  
  # Parametros iniciales
  #mu <- 100000 # valor inicial de mu
  #alfa <- 1000 # valor inicial de alfa
  #beta <- 1000 # valor inicial de beta
  
  # Parametros mejores
  mejor.pendiente <- pendiente.ini
  mejor.ordenada <- ordenada.ini
  
  y.real <- datos$bill_length_mm
  y.pred <- modelo.lineal(x = datos$body_mass_g,
                          ordenada = mejor.ordenada,
                          pendiente = mejor.pendiente)
  
  mejor.eval <- loss.fun(y.real, y.pred) # el mejor valor
  mejores <- matrix(c(mejor.eval, pendiente.ini, ordenada.ini, 1), 1, 4)
  k <- 0    # indice de iteracion
  actu <- 0 # indice de actualizacion
  
  # Historiales para graficar luego
  pendiente.hist <- c()
  ordenada.hist <- c()
  
  while (facred.acu > toler)
  {
    k <- k + 1
    # Genero nuevos valores aleatorios
    pendiente <-
      runif(1,
            mejor.pendiente - rango.pendiente * facred.acu,
            mejor.pendiente + rango.pendiente * facred.acu)
    
    ordenada <-
      runif(1,
            mejor.ordenada - rango.ordenada * facred.acu,
            mejor.ordenada + rango.ordenada * facred.acu)

    # Evaluacion de los nuevos valores
    y.pred <- modelo.lineal(x = datos$body_mass_g,
                            ordenada = mejor.ordenada,
                            pendiente = mejor.pendiente)
    valor <- loss.fun(y.real, y.pred)
    
    #cat(valor, "\n\n", y.pred, "\n\n", y.real)
    print(data.frame(y.real, y.pred))
    cat("Error:", valor)
    invisible(readline(prompt=paste0(k, " - Presione [enter] para seguir:")))
    
    if (valor < mejor.eval)
      # Si encuentro algo mejor -> Actualizacion
    {
      actu <- actu + 1
      
      mejor.eval <- valor
      mejor.pendiente <- pendiente
      mejor.ordenada <- ordenada
      
      pendiente.hist <- c(pendiente, pendiente.hist)
      ordenada.hist <- c(ordenada, ordenada.hist)
      
      mejores <- rbind(mejores, c(mejor.eval, pendiente, ordenada, k))
    }
    else
    # Si NO encuentro algo mejor -> Reduzco rango de busqueda
    {
      facred.acu <- facred.acu * facred
    }
  }
  #all.hist <- data.frame(mejor.eval, pendiente.hist, ordenada.hist)
  all.hist <- data.frame(pendiente.hist, ordenada.hist)
  return (list(mejores, all.hist))
}


## -------------------------------------------------------------------------------------
optim.aleatoria(datos = datos)

