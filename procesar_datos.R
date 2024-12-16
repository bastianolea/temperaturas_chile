# el script lee archivos csv de una carpeta "datos/originales/" y los procesa para crear un nuevo archivo 
# con información sobre temperaturas en chile. 
# se cargan los datos de estaciones meteorológicas desde otro archivo procesado en obtener_estaciones.R
# y luego se adicionan a la base de datos principales, se elimina datos perdidos y se convierten columnas a formato numérico.
# se guardan como resultado los datos procesados:  "datos/procesados/temperaturas_chile.csv" (csv), "datos/procesados/temperaturas_chile.rds" (rds)

library(dplyr)
library(purrr)
library(readr)
library(lubridate)

# obtener lista de datos scrapeados
archivos <- fs::dir_ls("datos/originales/", regexp = "csv")

# revisar archivos por año
map(2013:2024 |> set_names(), ~{
  año <- archivos |> 
    str_subset(as.character(.x))
  
  año |> str_extract("(?<=_)\\d{2}")
})


# # probar datos individuales
# readr::read_csv2(archivos[1])
# 
# read.csv2(archivos[20], colClasses = "character", header = FALSE) |> tibble()
# 
# read.csv2(archivos[20], colClasses = "character", header = FALSE) |> tibble() |> 
#   select(3)
# 
# read.csv2(archivos[10], colClasses = "character", header = FALSE) |> tibble()


# cargar todos los datos scrapeados
datos_0 <- map(archivos, read_delim, 
               delim = ";",
               col_names = FALSE, 
               col_types = cols(.default = col_character()))

# convertir a dataframe
datos_1 <- datos_0 |> 
  list_rbind() |> 
  tibble()

# renombrar columnas
datos_2 <- datos_1 |> 
  rename(codigo_nacional = 1,
         nombre = 2,
         latitud = 3,
         altura = 4, 
         año = 5, 
         mes = 6, 
         dia = 7,
         t_min = 8,
         t_max = 9)


# convertir a numéricos y eliminar filas con datos insuficientes
datos_3 <- datos_2 |> 
  mutate(across(c(altura, año, mes, dia, 
                  t_max, t_min), 
                as.numeric)) |> 
  filter(if_any(c(año, mes, dia, 
                  t_max, t_min),
                ~!is.na(.)))

# cargar datos de estaciones meteorológicas
estaciones <- read_csv2("datos/estaciones_meteorologicas_chile.csv", 
                               col_types = cols(.default = col_character())) |> 
  select(codigo_nacional, nombre, 
         latitud, longitud,
         zona_geografica)

# agregar datos de estaciones a los datos de temperaturas
datos_4 <- datos_3 |> 
  select(-latitud, -nombre) |> 
  left_join(estaciones,
            by = join_by(codigo_nacional))

# crear fecha
datos_5 <- datos_4 |> 
  mutate(fecha = ymd(paste(año, mes, dia)))

# coordenadas
datos_6 <- datos_5 |> 
  mutate(across(c(latitud, longitud), 
              ~stringr::str_replace(.x, ",", "\\.")),
       across(c(latitud, longitud), 
              as.numeric))


# guardar
datos_6 |> write_csv2("datos/procesados/temperaturas_chile.csv")

datos_6 |> write_rds("datos/procesados/temperaturas_chile.rds")
