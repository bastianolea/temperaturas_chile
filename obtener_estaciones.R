# el script de extrae información de estaciones meteorológicas desde la web
# se carga la url y se leen los datos utilizando la función read_html 
# y luego selecciona la tabla correcta para limpiar sus nombres con {janitor}
# se almacena como archivo "datos/estaciones_meteorologicas_chile.csv" 

# https://climatologia.meteochile.gob.cl/application/informacion/buscadorEstaciones

library(rvest)

# obtener mediante web scraping
url <- "https://climatologia.meteochile.gob.cl/application/informacion/buscadorEstaciones"

sitio <- session(url) |> 
  read_html()

tablas <- sitio |> 
  html_table()

estaciones <- tablas[[2]] |> 
  janitor::clean_names()

# guardar
write.csv2(estaciones,
           "datos/estaciones_meteorologicas_chile.csv")

# guardar en app
arrow::write_parquet(estaciones, "temperaturas_chile/estaciones.parquet")
