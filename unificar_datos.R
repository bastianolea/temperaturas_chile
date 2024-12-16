datos_5

datos <- readr::read_rds("datos/procesados/temperaturas_chile.rds")
datos_h <- readr::read_rds("datos/procesados/temperaturas_historicas_chile.rds")


datos_unificados <- bind_rows(datos,
                              datos_h |> filter(!año %in% unique(datos$año))
                              )

datos_unificados

# guardar
readr::write_csv2(datos_unificados,
                  "datos/procesados/temperaturas_chile_unificadas.csv")

# guardar para app
arrow::write_parquet(datos_unificados,
                     "temperaturas_chile/temperaturas.parquet")
