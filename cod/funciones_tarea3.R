#ejercicio 1


# Recive un df, ventana y k.alerta 
# Devuelve fecha, n.tx, media, desv y alerta.

alerta.movil <- function(df, ventana, k.alerta){
  
  if(!is.data.frame(df)){
    stop("df debe ser un data frame")
  }
  if(!is.numeric(ventana) || length(ventana) != 1){
    stop("ventana tiene que ser numérico")
  }
  if(!is.numeric(k.alerta) || length(k.alerta) != 1){
    stop("El mutiplicador de la desviación estándar para determinar si hay 
         alerta debe ser numérico y no negativo")
  }
  
  datos <- df |>
   dplyr::group_by(fecha) |>
   dplyr::summarise(n.tx = dplyr::n(),
                    .groups = "drop") |>
    dplyr::arrange(fecha)
  
  x <- 1/(ventana-1)
  
  datos$media <- NA_real_
  datos$desv <- NA_real_
  datos$alerta <- NA
  
  
  for(t in (ventana + 1):365){
    
    suma1 <- 0
    
    for(j in (t - ventana):(t - 1)){
      
      suma1 <- suma1 + datos$n.tx[j]
    
    }
    
    datos$media[t] <- suma1 / ventana
    
    suma2 <- 0
    
    for(j in (t-ventana):(t-1)){
      
      suma2 <- suma2 + (datos$n.tx[j] - datos$media[t])^2
      
    }
    
    datos$desv[t] <- sqrt(x*suma2)
    
      if(datos$n.tx[t] > (datos$media[t] + k.alerta * datos$desv[t])){
        datos$alerta[t] <- TRUE
      } else{
        datos$alerta[t] <- FALSE
      }
  }
  
  return(datos)
    
  }
  





