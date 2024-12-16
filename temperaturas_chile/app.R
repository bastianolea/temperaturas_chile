library(shiny)
library(bslib)
library(htmltools)

library(arrow)
library(dplyr)
library(ggplot2)
library(sf)
library(lubridate)

# cargar datos
datos <- read_parquet("temperaturas.parquet")
estaciones <- read_parquet("estaciones.parquet")

lista_estaciones <- estaciones |> 
  select(nombre, codigo_nacional) |> 
  tibble::deframe()

# ui ----
ui <- page_fluid(
  
  selectInput("estacion", 
              "Estación Meteorológica",
              choices = lista_estaciones,
              selected = "330020", 
              width = "100%"),
  
  card(
    card_header(),
    
    navset_card_underline(
      nav_panel("Mes",
                
                sliderInput("mes",
                            label = "Mes",
                            min = 1, max = 12,
                            value = 3, width = "100%"),
                
                div(style = css(min_width = "600px"),
                    plotOutput("grafico_promedio_mes")
                )
      ),
      nav_panel("Año",
                
                div(style = css(min_width = "600px"),
                    plotOutput("grafico_promedio_anual")
                )
      )
    )
  ),
  
  card(
    card_header(),
    
    div(style = css(min_width = "600px"),
        plotOutput("grafico_promedio_anual_estacion",
                   height = 250)
    )
  ),
  
  card(
    card_header(),
    div(style = css(min_width = "600px"),
        plotOutput("mapa_diferencia",
                   height = 500)
    )
  ),
  
  div(
    sliderInput("año",
                label = "Año",
                min = min(datos$año), max = 2023,
                sep = "",
                value = 1970, width = "100%")
  ),
  
  layout_columns(
    col_widths = c(6, 6),
    card(
      card_header(),
      
      plotOutput("grafico_radial_total")
    ),
    
    card(
      card_header(),
      plotOutput("grafico_radial_mensual")
    )
  ),
  
  
  layout_columns(
    col_widths = c(5, 7),
    card(
      card_header(),
      plotOutput("grafico_radial_año")
    ),
    
    card(
      card_header(),
      
      sliderInput("mes_zoom",
                  label = "Meses",
                  min = 1, max = 12,
                  value = c(1, 4), 
                  width = "100%"),
      
      div(style = css(min_width = "600px"),
          plotOutput("grafico_lineal_mes")
      )
    )
  ),
  
  
  
)

server <- function(input, output) {
  
  
  dato_estacion <- reactive({
    datos |> 
      filter(codigo_nacional == input$estacion)
  })
  
  # promedio mensual ----
  # máximas y mínimas, promedio mensual de una estación
  
  output$grafico_promedio_mes <- renderPlot({
    datos_mes <- dato_estacion() |> 
      filter(mes == input$mes) |> 
      filter(año >= 2000) |> 
      # filter(codigo_nacional == "330021") |>
      group_by(codigo_nacional, año) |>
      summarize(t_max = mean(t_max, na.rm = T),
                t_min = mean(t_min, na.rm = T))
    
    datos_mes |> 
      ggplot() +
      aes(x = año) +
      # segmento
      geom_segment(aes(xend = año, y = t_min, yend = t_max),
                   linewidth = 7, lineend = "round", alpha = .1) +
      # puntos
      geom_point(aes(y = t_max, color = t_max), size = 4) +
      geom_point(aes(y = t_min, color = t_min), size = 4) +
      # textos
      geom_text(aes(label = año, #round(t_max-t_min, 1),
                    y = (t_max+t_min)/2), 
                size = 3, angle = -90, alpha = .3, fontface = "bold") +
      geom_text(aes(label = round(t_max, 1), color = t_max,
                    y = t_max + max(t_max)*0.03), size = 3) +
      geom_text(aes(label = round(t_min, 1), color = t_min,
                    y = t_min - max(t_max)*0.03), size = 3) +
      # escalas
      scale_y_continuous(expand = expansion(c(0.1, 0.1))) +
      scale_x_continuous(breaks = unique(datos_mes$año)) +
      scale_color_gradient(low = "#3A88E3", high = "#D03447") +
      theme_void() +
      guides(color = guide_none())
    # theme(axis.text.x = element_text(face = "bold", angle = -90, margin = margin(b = 10)))
  })
  
  
  # promedio anual ----
  # máximas y mínimas, promedio anual de una estación
  
  
  output$grafico_promedio_anual <- renderPlot({
    datos_año <- dato_estacion() |> 
      filter(año >= 2000) |> 
      # filter(codigo_nacional == "330021") |>
      group_by(codigo_nacional, año) |>
      summarize(t_max = mean(t_max, na.rm = T),
                t_min = mean(t_min, na.rm = T))
    
    datos_año |> 
      ggplot() +
      aes(x = año) +
      # segmento
      geom_segment(aes(xend = año, y = t_min, yend = t_max),
                   linewidth = 7, lineend = "round", alpha = .1) +
      # puntos
      geom_point(aes(y = t_max, color = t_max), size = 4) +
      geom_point(aes(y = t_min, color = t_min), size = 4) +
      # textos
      geom_text(aes(label = año, #round(t_max-t_min, 1),
                    y = (t_max+t_min)/2), 
                size = 3, angle = -90, alpha = .3, fontface = "bold") +
      geom_text(aes(label = round(t_max, 1), color = t_max,
                    y = t_max + max(t_max)*0.03), size = 3) +
      geom_text(aes(label = round(t_min, 1), color = t_min,
                    y = t_min - max(t_max)*0.03), size = 3) +
      # escalas
      scale_y_continuous(expand = expansion(c(0.1, 0.1))) +
      scale_x_continuous(breaks = unique(datos_año$año)) +
      scale_color_gradient(low = "#3A88E3", high = "#D03447") +
      theme_void() +
      guides(color = guide_none())
    # theme(axis.text.x = element_text(face = "bold", angle = -90, margin = margin(b = 10)))
  })
  
  
  
  # promedio anual de una estación ----
  
  output$grafico_promedio_anual_estacion <- renderPlot({
    datos_año_prom <- dato_estacion() |> 
      filter(año >= 2000) |> 
      # filter(codigo_nacional == "330021") |>
      group_by(codigo_nacional, año) |>
      summarize(t_max = mean(t_max, na.rm = T),
                t_min = mean(t_min, na.rm = T)) |> 
      mutate(t_med = (t_max + t_min)/2)
    
    datos_año_prom |> 
      ggplot() +
      aes(x = año, y = t_med, color = t_med) +
      # segmento
      geom_segment(aes(xend = año, y = 0, yend = t_med),
                   linewidth = 0.3, alpha = .3) +
      geom_line(linewidth = 1.2, alpha = .3) +
      # textos
      geom_label(aes(label = round(t_med, 1), 
                     y = t_med + max(t_med)*0.06), 
                 size = 2.5, label.size = NA) +
      # puntos
      geom_point(size = 4) +
      # escalas
      scale_color_gradient(low = "#3A88E3", high = "#D03447") +
      scale_y_continuous(labels = ~paste0(.x, "°"),
                         expand = expansion(c(0.015, 0.2))) +
      scale_x_continuous(breaks = unique(datos_año_prom$año)) +
      guides(color = guide_none()) +
      theme_void() +
      theme(axis.text.x = element_text(face = "bold", angle = -90, margin = margin(b = 10)),
            axis.text.y = element_text(margin = margin(l = 7, r = -8)),
            panel.grid.major.y = element_line(linetype = "dashed", color = "grey95"))
  })
  
  
  # diferencia mapa ----
  datos_mapa <- datos |> 
    filter(año >= 2016) |> 
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
    filter(if_any(c(latitud, longitud), 
                  ~!is.na(.x))) |> 
    st_as_sf(coords = c("longitud", "latitud"), 
             crs = 4326)
  
  
  estaciones_filt <- estaciones |> 
    filter(codigo_nacional %in% c(200006,
                                  230001,
                                  270008,
                                  290004,
                                  330020,
                                  360019,
                                  390006,
                                  410005,
                                  450001,
                                  520006)) |> 
    mutate(ciudad = case_match(codigo_nacional,
                               200006 ~ "Iquique",
                               230001 ~ "Antofagasta",
                               270008 ~ "Atacama",
                               290004 ~ "La Serena",
                               330020 ~ "Santiago",
                               360019 ~ "Concepción",
                               390006 ~ "Valdivia",
                               410005 ~ "Pto. Montt",
                               450001 ~ "Aysén",
                               520006 ~ "Pta. Arenas"))
  
  output$mapa_diferencia <- renderPlot({
    datos_mapa |> 
      ggplot() +
      # líneas horizontales
      geom_hline(yintercept = estaciones_filt$latitud,
                 colour = "grey85", linetype = "dashed", linewidth = 0.3) +
      # puntos
      geom_sf(aes(size = diferencia, color = diferencia), alpha = .8) +
      # escalas
      scale_color_gradient(low = "#3A88E3", high = "#D03447",
                           labels = ~paste0(format(.x, decimals = 1, decimal.mark = ","), "°"),
                           breaks = seq(0, 3, by = 0.5)) +
      scale_size(range = c(3, 8),
                 labels = ~paste0(format(.x, decimals = 1, decimal.mark = ","), "°"),
                 breaks = seq(0, 3, by = 0.5)) +
      scale_x_continuous(expand = expansion(c(0.2, 0))) +
      scale_y_continuous(breaks = estaciones_filt$latitud,
                         labels = estaciones_filt$ciudad) +
      coord_sf(expand = TRUE, clip = "off") +
      facet_wrap(~año, nrow = 1) +
      # temas
      theme_void() +
      guides(color = guide_legend(title = "Diferencia:",position = "bottom", nrow = 1),
             size = guide_legend(title = "Diferencia:",
                                 override.aes = list(alpha = 0.3))) +
      theme(plot.margin = unit(c(0, 3, 0, 2), "mm"),
            strip.text = element_text(face = "bold", size = 10,
                                      margin = margin(b = 4)),
            panel.spacing.x = unit(5, "mm"),
            panel.grid.major.y = element_line(color = "red"),
            legend.title = element_text(face = "italic"),
            legend.text = element_text(margin = margin(l = 2, r = 4)),
            axis.text.y = element_text(colour = "grey80",
                                       hjust = 1, size = 7, 
                                       margin = margin(r = 2)))
  })
  
  
  dato_estacion_año <- reactive({
  dato_estacion() |> 
    filter(año >= input$año)
  })
    
  # radial -----
  output$grafico_radial_total <- renderPlot({
    datos_radial_total <- dato_estacion_año() |> 
      # filter(codigo_nacional == "330021") |>
      mutate(fecha_x = ymd(paste(2024, mes, dia)))
    
    datos_radial_total |> 
      ggplot() +
      aes(x = fecha_x, y = t_max) +
      # escala interna y
      annotate("label", label = c("10°", "25°", "35°"), y = c(10, 25, 35), 
               x = as_date("2024-02-15"), label.size = NA,
               size = 3, angle = 90, color = "grey70") +
      # líneas
      geom_line(aes(group = año, alpha = año, color = t_max), 
                linewidth = 0.4) + 
      # escalas
      scale_color_gradient(low = "#3A88E3", high = "#D03447") +
      scale_x_date(date_breaks = "month", date_labels = "%m",
                   expand = expansion(0)) +
      # temas
      coord_radial(rotate.angle = T, inner.radius = 0.1) +
      theme_void() +
      theme(axis.text.x = element_text(face = "bold"),
            panel.grid.major.x = element_line(linetype = "dashed", colour = "grey95"),
            panel.grid.major.y = element_line(colour = "grey95")) +
      theme(legend.position = "none")
  })
  
  
  # destacar ultimo año ----
  
  datos_radial_año <- reactive({
    dato_estacion_año() |> 
      # filter(codigo_nacional == "330021") |>
      mutate(fecha_x = ymd(paste(2024, mes, dia))) |> 
      mutate(actual = ifelse(año == 2023, "actual", "anterior"))
  })
  
  # datos |> 
  output$grafico_radial_año <- renderPlot({
    
    datos_radial_año() |> 
      ggplot() +
      aes(x = fecha_x, y = t_max) +
      # escala interna y
      annotate("label", label = c("10°", "25°", "35°"), y = c(10, 25, 35), 
               x = as_date("2024-02-15"), label.size = NA,
               size = 3, angle = 90, color = "grey70") +
      # líneas
      geom_line(aes(group = año, alpha = actual, linewidth = actual, color = t_max)) + 
      # escalas
      scale_color_gradient(low = "#3A88E3", high = "#D03447") +
      scale_x_date(date_breaks = "month", date_labels = "%m",
                   expand = expansion(0)) +
      scale_alpha_manual(values = c("actual" = 1, "anterior" = 0.3)) +
      scale_linewidth_manual(values = c("actual" = 0.8, "anterior" = 0.2)) +
      # temas
      coord_radial(rotate.angle = T, inner.radius = 0.1) +
      theme_void() +
      theme(axis.text.x = element_text(face = "bold"),
            panel.grid.major.x = element_line(linetype = "dashed", colour = "grey95"),
            panel.grid.major.y = element_line(colour = "grey95")) +
      theme(legend.position = "none")
  })
  
  datos_radial_mes <- reactive({
    dato_estacion_año() |> 
      # filter(codigo_nacional == "330020") |>
      group_by(año, mes) |>
      summarize(t_max = mean(t_max, na.rm = T),
                t_min = mean(t_min, na.rm = T)) |>
      ungroup() |> 
      mutate(fecha = ymd(paste(año, mes, 15))) |>
      mutate(fecha_x = ymd(paste(2024, mes, 1))) |>
      # mutate(fecha_x = ymd(paste(2024, mes, ifelse(mes == 1, 1, 30)))) |>
      mutate(actual = ifelse(año == 2023, "actual", "anterior"))
  })
  
  ## lineal mes ----
  output$grafico_lineal_mes <- renderPlot({
    datos_radial_año() |> 
      filter(mes >= input$mes_zoom[1], mes <= input$mes_zoom[2]) |> 
      ggplot() +
      aes(x = fecha_x, y = t_max) +
      # líneas
      geom_line(aes(group = año, alpha = actual, linewidth = actual, color = t_max)) + 
      # escalas
      scale_color_gradient(low = "#3A88E3", high = "#D03447") +
      scale_x_date(date_breaks = "month", date_labels = "%m",
                   expand = expansion(c(0.05, 0.05))) +
      scale_y_continuous(labels = ~paste0(.x, "°")) +
      scale_alpha_manual(values = c("actual" = 1, "anterior" = 0.3)) +
      scale_linewidth_manual(values = c("actual" = 0.8, "anterior" = 0.2)) +
      # temas
      # coord_radial(rotate.angle = T, inner.radius = 0.1) +
      theme_void() +
      theme(axis.text.x = element_text(face = "bold", margin = margin(b = 10)),
            axis.text.y = element_text(margin = margin(l = 7)),
            panel.grid.major.x = element_line(linetype = "dashed", colour = "grey95"),
            panel.grid.major.y = element_line(colour = "grey95")) +
      theme(legend.position = "none")
  })
  
  # radial por mes ----
  # datos |> 
  output$grafico_radial_mensual <- renderPlot({
    # browser()
    datos_radial_mes() |> 
      ggplot() +
      aes(x = fecha_x, y = t_max) +
      # escala interna y
      annotate("label", label = c("10°", "25°", "35°"), y = c(10, 25, 35), 
               x = as_date("2024-02-15"), label.size = NA,
               size = 3, angle = 90, color = "grey70") +
      # líneas
      geom_line(aes(group = año, color = t_max,
                    alpha = actual, linewidth = actual)) + 
      # escalas
      scale_color_gradient(low = "#3A88E3", high = "#D03447") +
      scale_x_date(date_breaks = "month", date_labels = "%m",
                   expand = expansion(0)) +
      scale_y_continuous(breaks = c(10, 25, 35), limits = c(0, 35), 
                         expand = expansion(c(0, 0.1))) +
      scale_alpha_manual(values = c("actual" = 1, "anterior" = 0.2)) +
      scale_linewidth_manual(values = c("actual" = 0.8, "anterior" = 0.2)) +
      # temas
      coord_radial(rotate.angle = T, inner.radius = 0.1) +
      theme_void() +
      theme(axis.text.x = element_text(face = "bold"),
            panel.grid.major.x = element_line(linetype = "dashed", colour = "grey95"),
            panel.grid.major.y = element_line(colour = "grey95")) +
      theme(legend.position = "none")
  })
  
}

shinyApp(ui = ui, server = server)
