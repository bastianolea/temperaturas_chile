library(ggplot2)
library(dplyr)

datos <- read_rds("datos/procesados/temperaturas_chile.rds")

# histogramas ----
datos |> 
  ggplot() +
  aes(x = fecha) +
  geom_histogram(binwidth = 100)

datos |> 
  ggplot() +
  aes(x = fecha) +
  geom_histogram() +
  scale_x_date(date_breaks = "years", date_labels = "%Y")

datos |> 
  filter(year(fecha) == 2019) |> 
  ggplot() +
  aes(x = fecha) +
  geom_histogram() +
  scale_x_date(date_breaks = "months", date_labels = "%m")


datos |> 
  filter(codigo_nacional == "330020") |>
  filter(mes == 3) |> 
  group_by(codigo_nacional, año) |> 
  summarize(t_max = mean(t_max, na.rm = T)) |> 
  ggplot() +
  aes(x = año, y = t_max, color = codigo_nacional) +
  geom_line()
