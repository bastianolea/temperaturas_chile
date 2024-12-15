# este script obtiene los datos de temperatura históricas de la Dirección Meteorológica de Chile usando web scrapping.
# se realiza una alteración sobre el buscador del sitio web en base a los códigos nacionales de las estaciones meteorológicas y los meses del año
# luego los datos son limpiados y unificados en una sola base
# el resultado es el archivo temperaturas_historicas_chile.csv, también en formato .rds

library(rvest)
library(dplyr)
library(purrr)
library(tidyr)
library(janitor)
library(lubridate)

# https://climatologia.meteochile.gob.cl/application/historico/temperaturaHistoricaMes/330021/2

# cargar estaciones
estaciones <- readr::read_csv2("datos/estaciones_meteorologicas_chile.csv") |> 
  select(codigo_nacional, nombre, 
         latitud, longitud,
         zona_geografica)

codigos_estaciones <- unique(estaciones$codigo_nacional)

codigos_estaciones


# scraping ----
url_base <- "https://climatologia.meteochile.gob.cl/application/historico/temperaturaHistoricaMes"


# loop por todas las estaciones y por los doce meses para obtener datos históricos
datos_historicos_0 <- map(codigos_estaciones, \(.estacion) {
  
  message("estacion: ", .estacion)
  # .mes <- 1
  # .estacion <- as.character(codigos_estaciones[1])
  # .estacion <- "330163"
  
  datos_mes <- map(1:12, \(.mes) {
    message("     mes: ", .mes)
    
    # crear url
    url <- paste(url_base, .estacion, .mes, sep = "/")
    
    # scraping
    sitio <- session(url) |> 
      read_html()
    
    # Sys.sleep(0.3)
    
    tablas <- sitio |> 
      html_table(convert = FALSE)
    
    # limpiar tablas
    tabla <- tablas[[1]] |> 
      clean_names() |> 
      rename(año = 1)
    
    # salir si no hay datos
    if (nrow(tabla) < 20) return(NULL)
    
    t_min <- tabla |> 
      select(año, contains("minima")) |> 
      row_to_names(1) |> 
      clean_names() |> 
      rename(año = 1) |> 
      mutate(temperatura = "t_min")
    
    t_med <- tabla |> 
      select(año, contains("media")) |> 
      row_to_names(1) |> 
      clean_names() |> 
      rename(año = 1) |> 
      mutate(temperatura = "t_med") |> 
      rename(media = aritmetica,
             absoluta = climatologica) |> 
      select(-cantidad_de_datos)
    
    t_max <- tabla |> 
      select(año, contains("maxima")) |> 
      row_to_names(1) |> 
      clean_names() |> 
      rename(año = 1) |> 
      mutate(temperatura = "t_max")
    
    # unir datos
    datos_mes <- bind_rows(t_min,
                           t_med,
                           t_max) |> 
      mutate(mes = .mes)
    
    return(datos_mes)
  })
  
  # unir
  datos_estacion <- datos_mes |> 
    list_rbind() |> 
    mutate(codigo_nacional = .estacion)
  
  return(datos_estacion)
}) |> 
  list_rbind()



datos_historicos_1 <- datos_historicos_0 |> 
  # convertir a numéricos
  mutate(dia = as.numeric(dia),
         año = as.numeric(año)) |> 
  filter(!is.na(año))

# fechas
datos_historicos_2 <- datos_historicos_1 |> 
  # corregir datos sin día
  mutate(dia = if_else(is.na(dia), 15, dia)) |> 
  # crear fechas
  mutate(fecha = ymd(paste(año, mes, dia)))

# corregir fechas que no cuadran por tener 31 días
datos_historicos_3 <- datos_historicos_2 |>   
  mutate(fecha = if_else(is.na(fecha), 
                         ymd(paste(año, mes, dia-1)),
                         as_date(fecha)))

datos_historicos_3 |> 
  filter(is.na(fecha))

# agregar datos de estaciones
datos_historicos_4 <- datos_historicos_3 |> 
  left_join(estaciones,
          by = join_by(codigo_nacional))


# guardar ----
datos_historicos_4 |> readr::write_csv2("datos/procesados/temperaturas_historicas_chile.csv")

datos_historicos_4 |> readr::write_rds("datos/procesados/temperaturas_historicas_chile.rds")
