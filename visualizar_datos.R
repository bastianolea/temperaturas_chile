
# máximas y mínimas, promedio mensual de una estación
datos_mes <- datos |> 
  filter(mes == 2) |> 
  group_by(codigo_nacional, año) |>
  filter(codigo_nacional == "330021") |>
  summarize(t_max = mean(t_max, na.rm = T),
            t_min = mean(t_min, na.rm = T))

datos_mes |> 
  ggplot() +
  aes(x = año) +
  # segmento
  geom_segment(aes(xend = año, y = t_min, yend = t_max),
               linewidth = 8, lineend = "round", alpha = .1) +
  # puntos
  geom_point(aes(y = t_max), size = 5, color = "#D03447") +
  geom_point(aes(y = t_min), size = 5, color = "#3A88E3") +
  # textos
  geom_text(aes(label = round(t_max-t_min, 1),
                y = (t_max+t_min)/2), 
            size = 3, angle = -90, alpha = .2) +
  geom_text(aes(label = round(t_max, 1), 
                y = t_max + max(t_max)*0.03), size = 3) +
  geom_text(aes(label = round(t_min, 1), 
                y = t_min - max(t_max)*0.03), size = 3) +
  # escalas
  scale_y_continuous(expand = expansion(c(0.1, 0.1))) +
  scale_x_continuous(breaks = 2016:2024) +
  theme_void() +
  theme(axis.text.x = element_text(face = "bold", angle = -90, margin = margin(b = 10)))



# máximas y mínimas, promedio anual de una estación ----
datos_año <- datos |> 
  group_by(codigo_nacional, año) |>
  filter(codigo_nacional == "330021") |>
  summarize(t_max = mean(t_max, na.rm = T),
            t_min = mean(t_min, na.rm = T))

datos_año |> 
  ggplot() +
  aes(x = año) +
  # segmento
  geom_segment(aes(xend = año, y = t_min, yend = t_max),
               linewidth = 8, lineend = "round", alpha = .1) +
  # puntos
  geom_point(aes(y = t_max), size = 5, color = "#D03447") +
  geom_point(aes(y = t_min), size = 5, color = "#3A88E3") +
  # textos
  geom_text(aes(label = round(t_max-t_min, 1),
                y = (t_max+t_min)/2), 
            size = 3, angle = -90, alpha = .2) +
  geom_text(aes(label = round(t_max, 1), 
                y = t_max + max(t_max)*0.03), size = 3) +
  geom_text(aes(label = round(t_min, 1), 
                y = t_min - max(t_max)*0.03), size = 3) +
  # escalas
  scale_y_continuous(expand = expansion(c(0.1, 0.1))) +
  scale_x_continuous(breaks = 2016:2024) +
  theme_void() +
  theme(axis.text.x = element_text(face = "bold", angle = -90, margin = margin(b = 10)))




# promedio anual de una estación ----
datos_año_prom <- datos |> 
  group_by(codigo_nacional, año) |>
  filter(codigo_nacional == "330021") |>
  summarize(t_max = mean(t_max, na.rm = T),
            t_min = mean(t_min, na.rm = T)) |> 
  mutate(t_med = (t_max + t_min)/2)

datos_año_prom |> 
  ggplot() +
  aes(x = año) +
  # segmento
  geom_segment(aes(xend = año, y = t_min, yend = t_max),
               linewidth = 8, lineend = "round", alpha = .1) +
  # puntos
  geom_point(aes(y = t_med, color = t_med), size = 5) +
  # textos
  geom_text(aes(label = round(t_med, 1), 
                y = t_med + max(t_med)*0.05), size = 3) +
  # escalas
  scale_color_gradient(low = "#3A88E3", high = "#D03447") +
  scale_y_continuous(expand = expansion(c(0.1, 0.1))) +
  scale_x_continuous(breaks = 2016:2024) +
  guides(color = guide_none()) +
  theme_void() +
  theme(axis.text.x = element_text(face = "bold", angle = -90, margin = margin(b = 10)))



datos |> count(zona_geografica)

# lineas zona ----
datos |> 
  filter(año >= 2022) |> 
  group_by(codigo_nacional, año, mes, zona_geografica) |>
  summarize(t_max = mean(t_max, na.rm = T),
            t_min = mean(t_min, na.rm = T)) |> 
  mutate(fecha = ymd(paste(año, mes, 15))) |> 
  ggplot() +
  aes(x = fecha, y = t_max) +
  geom_line(aes(group = codigo_nacional, color = t_max)) +
  scale_color_gradient(low = "#3A88E3", high = "#D03447") +
  facet_wrap(~zona_geografica, nrow = 1)


# diferencia mapa ----
library(sf)
datos_mapa <- datos |> 
  filter(!codigo_nacional %in% c("270001", "950001", "330031")) |> 
  group_by(codigo_nacional, año) |>
  summarize(t_max = mean(t_max, na.rm = T),
            t_min = mean(t_min, na.rm = T),
            latitud = first(latitud),
            longitud = first(longitud)) |> 
  # calcular diferencia
  arrange(codigo_nacional, año) |> 
  mutate(diferencia = t_max-lag(t_max),
         diferencia = ifelse(is.na(diferencia), 0, diferencia),
         diferencia = ifelse(diferencia < 0, 0, diferencia)) |> 
  # coordenadas
  mutate(across(c(latitud, longitud), 
                ~stringr::str_replace(.x, ",", "\\.")),
         across(c(latitud, longitud), 
                as.numeric)) |> 
  filter(if_any(c(latitud, longitud), 
                ~!is.na(.x))) |> 
  st_as_sf(coords = c("longitud", "latitud"), 
           crs = 4326)



datos_mapa |> 
  ggplot() +
  geom_sf(aes(size = diferencia, color = diferencia), alpha = .8) +
  scale_color_gradient(low = "#3A88E3", high = "#D03447",
                       labels = ~paste0(format(.x, decimals = 1, decimal.mark = ","), "°"),
                       breaks = seq(0, 3, by = 0.5)) +
  scale_size(range = c(3, 8),
             labels = ~paste0(format(.x, decimals = 1, decimal.mark = ","), "°"),
             breaks = seq(0, 3, by = 0.5)) +
  coord_sf(expand = TRUE, clip = "off") +
  scale_x_continuous(expand = expansion(c(0.2, 0))) +
  facet_wrap(~año, nrow = 1) +
  theme_void() +
  guides(color = guide_legend(), size = guide_legend(override.aes = list(alpha = 0.3))) +
  theme(plot.margin = unit(c(0, 0, 0, 4), "mm"))



# radial -----
datos |> 
  # filter(año >= 2022) |> 
  group_by(codigo_nacional, año, mes, zona_geografica) |>
  summarize(t_max = mean(t_max, na.rm = T),
            t_min = mean(t_min, na.rm = T)) |> 
  mutate(fecha = ymd(paste(año, mes, 15))) |> 
  mutate(grupo = paste(año, codigo_nacional)) |> 
  ggplot() +
  aes(x = mes, y = t_max) +
  geom_line(aes(group = grupo, alpha = año, color = t_max)) +
  scale_color_gradient(low = "#3A88E3", high = "#D03447") +
  scale_alpha_continuous(range = c(0.1, 1)) +
  facet_wrap(~zona_geografica, nrow = 2) +
  coord_polar()


# radial -----
datos |> 
  filter(codigo_nacional == "330021") |>
  mutate(fecha_x = ymd(paste(2024, mes, dia))) |> 
  ggplot() +
  aes(x = fecha_x, y = t_max) +
  geom_line(aes(group = año, alpha = año, color = t_max), 
            linewidth = 0.3) + 
  scale_color_gradient(low = "#3A88E3", high = "#D03447") +
  scale_x_date(date_breaks = "month", date_labels = "%m") +
  scale_alpha_continuous(range = c(0.1, 1)) +
  coord_polar() +
  ylim(c(0, 40)) +
  theme_void() +
  theme(axis.text.x = element_text())

# destacar ultimo año ----
datos |> 
  filter(codigo_nacional == "330021") |>
  mutate(fecha_x = ymd(paste(2024, mes, dia))) |> 
  mutate(actual = ifelse(año >= 2023, "actual", "anterior")) |> 
  ggplot() +
  aes(x = fecha_x, y = t_max) +
  geom_line(aes(group = año, alpha = actual, linewidth = actual, color = t_max)) + 
  scale_color_gradient(low = "#3A88E3", high = "#D03447") +
  scale_x_date(date_breaks = "month", date_labels = "%m") +
  scale_alpha_manual(values = c("actual" = 1, "anterior" = 0.2)) +
  scale_linewidth_manual(values = c("actual" = 0.5, "anterior" = 0.2)) +
  coord_polar() +
  ylim(c(0, 40)) +
  theme_void() +
  theme(axis.text.x = element_text())


# radial por mes ----
datos |> 
  filter(codigo_nacional == "330020") |>
  group_by(año, mes) |>
  summarize(t_max = mean(t_max, na.rm = T),
            t_min = mean(t_min, na.rm = T)) |>
  mutate(fecha = ymd(paste(año, mes, 15))) |>
  mutate(fecha_x = ymd(paste(2024, mes, 15))) |> 
  mutate(actual = ifelse(año >= 2023, "actual", "anterior")) |> 
  ggplot() +
  aes(x = fecha_x, y = t_max) +
  geom_line(aes(group = año, alpha = actual, color = t_max), 
            linewidth = 0.3) + 
  scale_color_gradient(low = "#3A88E3", high = "#D03447") +
  scale_x_date(date_breaks = "month", date_labels = "%m") +
  scale_alpha_manual(values = c("actual" = 1, "anterior" = 0.2)) +
  scale_linewidth_manual(values = c("actual" = 0.5, "anterior" = 0.2)) +
  coord_polar() +
  ylim(c(0, 40)) +
  theme_void() +
  theme(axis.text.x = element_text())
