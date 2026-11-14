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
  


#ejercicio 2


# Función 1
# Recibe: un vector numérico
# Devuelve: MAD de un vector de montos

benford.mad <- function(x){
  
  d <-  (floor(x/10^(floor(log10(x)))))
  
  B.d <-  log10(1+(1/(1:9)))
  
  frecuencia <- table(factor(d, levels = 1:9))
  
  p.d <- as.numeric(frecuencia) / length(d)
  
  mad <- sum(abs(p.d - B.d)) / 9
  
}

# Función 2
# Recive: data frame, canal, cantones
# Devuelve: 

benford.canton <- function(df, canal, cantones){
  
  datos <- df |>
    dplyr::filter(tolower(.data$canal) == tolower(.env$canal)) |>
    dplyr::group_by(cod_canton) |>
    dplyr::summarise(n = dplyr::n(),
                     mad = benford.mad(monto_crc),
                     .groups = "drop") |>
    dplyr::left_join(cantones, by = "cod_canton") |>
    dplyr::mutate(clase = dplyr::case_when(
      mad <= 0.006 ~ "conformidad cercana",
      mad > 0.006 & mad <= 0.012 ~ "conformidad aceptable",
      mad > 0.012 & mad <= 0.015 ~ "conformidad marginal",
      .default = "no conformidad"
    )) |>
    dplyr::arrange(dplyr::desc(mad))
  
  
  return(datos)
  
  
}






