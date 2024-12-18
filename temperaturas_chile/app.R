library(shiny)
library(bslib)
library(htmltools)
library(arrow)
library(dplyr)
library(ggplot2)
library(sf)
library(lubridate)
library(shinycssloaders)

# cargar datos
datos <- read_parquet("temperaturas.parquet")
estaciones <- read_parquet("estaciones.parquet")

# lista_estaciones <- estaciones |> 
#   select(nombre, codigo_nacional) |> 
#   tibble::deframe()

lista_estaciones <- datos |>
  select(nombre, codigo_nacional) |> 
  distinct() |> 
  tibble::deframe()

# colores
color <- list(alto = "#D03447",
              bajo = "#3A88E3",
              detalle = "#63485e", #"grey80",
              fondo = "#271f25", #"#2d232b",
              texto = "#bfa2ba" #"#c8b9c5" #"#c2c3c7"
)

# opciones
resolucion = 90
options(spinner.type = 8, spinner.color = color$detalle)

# ui ----
ui <- page_fluid(
  title = "Temperaturas extremas en Chile",
  lang = "es",
  
  ## tema ----
  theme = bs_theme(bg = color$fondo,
                   fg = color$texto,
                   primary = color$alto,
                   secondary = color$alto) |> 
    bs_add_rules(c(
      paste(".card {overflow: visible !important; 
                   border: 1px solid", color$detalle, ";
                   border-radius: 5px; }"))
    ),
  
  # líneas internas de sliders
  tags$style(
    HTML(".irs-line, .irs-grid-pol {background-color:", color$detalle ,"!important;}")
  ),
  
  # borde de selector
  tags$style(
    HTML(".selectize-input {border: 1px solid", color$detalle, ";}")),
  
  br(),
  
  # header ----
  div(style = css(max_width = "1200px", margin = "auto", margin_bottom = "20px"),
      div(
        h1("Temperaturas extremas en Chile"),
        
        div(style = css(margin_top = "-4px", margin_bottom = "20px",
        ),
        a(em("Bastián Olea Herrera"), 
          href = "https://bastianolea.rbind.io",
          style = css(color = color$detalle))
        ),
        
        markdown("Visualización de datos históricos de temperaturas extremas en el país."),
        markdown("Los datos fueron obtenidos desde la Dirección General de Aeronáutica Civil, por medio de la [plataforma de Datos Abiertos del Estado](https://datos.gob.cl/dataset/?q=temperatura), y desde la [Dirección Meteorológica de Chile](https://climatologia.meteochile.gob.cl) mediante web scraping.")
      ),
      
      br(),
      
      div(style = css(margin_bottom = "48px"),
          selectInput("estacion", 
                      "Estación Meteorológica",
                      choices = c(lista_estaciones),
                      selected = "330020", 
                      width = "400px"),
          
          em("Seleccione una estación meteorológica para visualizar sus datos en los gráficos a continuación. La selección afectará a todos los gráficos, excepto los mapas.")
      )
  ),
  
  ## primera fila ----
  card(style = css(max_width = "1000px", margin = "auto", margin_bottom = "20px"),
       card_header(
         h2("Temperaturas máximas y mínimas por estación"),
       ),
       
       p("Comparación anual de temperaturas máximas y mínimas de la estación meteorológica seleccionada."),
       
       navset_card_underline(
         
         nav_panel("Mes",
                   
                   card_header(
                     h4("Temperaturas máximas y mínimas mensuales"),
                   ),
                   
                   div(style = css(margin = "auto"),
                       sliderInput("mes",
                                   label = "Mes",
                                   min = 1, max = 12,
                                   value = 3, width = "400px")
                   ),
                   
                   em("Cada barra vertical representa un año, y los puntos en sus extremos representan la temperatura máxima y mínima del mes seleccionado."),
                   
                   div(style = css(min_width = "600px"),
                       plotOutput("grafico_promedio_mes") |> withSpinner()
                   ),
                   
                   em("Por ejemplo, si se selecciona el mes 3 (marzo), los puntos representan las temperaturas máximas y mínimas de la estación seleccionada, en el mes de marzo, en cada año."),
         ),
         nav_panel("Año",
                   
                   card_header(
                     h4("Temperaturas máximas y mínimas anuales"),
                   ),
                   
                   em("Cada barra vertical representa un año, y los puntos en sus extremos representan la temperatura máxima y mínima del año."),
                   
                   div(style = css(min_width = "600px"),
                       plotOutput("grafico_promedio_anual") |> withSpinner()
                   )
         )
       )
  ),
  
  ## segunda fila ----
  card(style = css(max_width = "1000px", margin = "auto", margin_bottom = "20px"),
       card_header(
         h2("Temperaturas máximas mensuales, histórico")
       ),
       
       em("Gráfico de datos históricos, donde la línea zigzagueante representa la temperatura máxima mensual, y su evolución a través de las décadas."),
       
       div(style = css(min_width = "600px"),
           plotOutput("grafico_promedio_anual_estacion",
                      height = 350) |> withSpinner()
       )
  ),
  
  
  ## mapa ----
  card(style = css(max_width = "1000px", margin = "auto", margin_bottom = "20px"),
       card_header(
         h2("Aumento de temperaturas máximas por año, por estación")
       ),
       
       em("En esta visualización se representan las ubicaciones geográficas de las estaciones meteorológicas por medio de puntos, y hacia el lado se van mostrando las mismas estaciones, año a año. El tamaño y color de los puntos representa la diferencia de temperatura máxima registrada con respecto al año anterior."),
       
       div(style = css(min_width = "700px"),
           plotOutput("mapa_diferencia",
                      height = 450) |> withSpinner()
       ),
       
       em("Un punto más grande y rojizo significa que en ese año, la temperatura máxima registrada en esa estación fue superior a la máxima del año anterior."),
  ),
  
  
  ## radiales ----
  card_body(
    div(style = css(margin = "auto"),
        # div(#style = css(margin = "auto"),
        sliderInput("año",
                    label = "Año de inicio",
                    min = min(datos$año), max = 2024,
                    sep = "", step = 1,
                    value = 1970, width = "600px")
        # )
    )),
  
  
  layout_columns(
    col_widths = c(6, 6),
    
    card(full_screen = TRUE, 
         
         card_header(
           h2("Temperaturas máximas diarias, por estación")
         ),
         
         em("Este gráfico radial muestra las temperaturas máximas diarias, dispuestas como un anillo. El círculo completo representa un año, dividido en meses."),
         
         div(style = css(margin = "auto"),
             div(style = css(min_width = "500px", margin = "auto"),
                 plotOutput("grafico_radial_total",
                            height = 500, width = 500) |> withSpinner(),
             )
         ),
         # div(style = css(max_height = "500px", overflow_y = "hidden"),
         #         plotOutput("grafico_radial_total",
         #                    height = "600px") |> withSpinner()
         # ),
         
         em("Las líneas se sobreponen debido a que en un mismo anillo se visualizan las temperaturas de todos los años, desde el año seleccionado hasta el presente."),
    ),
    
    card(full_screen = TRUE, min_height = "800px",
         card_header(
           h2("Temperaturas promedio mensuales, por estación")
         ),
         
         em("Las líneas que envuelven esta visualización radial representan las temperaturas máximas promedio de cada mes, a partir del año seleccionado hasta el presente. El promedio mensual presenta los datos de forma simplificada, permitiendo ver con claridad que los promedios mensuales recientes (en línea más gruesa) son superiores a los de las décadas anteriores."),
         
         div(style = css(margin = "auto"),
             div(style = css(min_width = "500px", margin = "auto"),
                 plotOutput("grafico_radial_mensual",
                            height = 500, width = 500) |> withSpinner()
             )
         )
         # plotOutput("grafico_radial_mensual",
         #            width = "100%") |> withSpinner()
    )
  ),
  
  ### ultima fila ----
  card(
    card_header(
      h2("Temperaturas máximas diarias, por estación")
    ),
    
    p("Visualización de temperaturas diarias, filtrando años y meses."),
    
    div(style = css(margin = "auto"),
        sliderInput("año_zoom",
                    label = "Año",
                    min = 2016, 
                    max = max(datos$año), 
                    value = 2023, sep = "",
                    width = "600px")
    ),
    
    em("Seleccione un año para destacarlo en las siguientes dos visualizaciones. El año seleccionado aparecerá como una línea más gruesa que el resto. El resto de líneas corresponde a los años anteriores, como una forma de comparar el año seleccionado con el histórico de la estación meteorológica."),
    
    layout_columns(
      col_widths = c(6, 6),
      
      card(full_screen = TRUE,
           card_header(
             h4("Temperaturas diarias, anuales")
           ),
           
           div(style = css(margin = "auto"),
               div(style = css(min_width = "500px", margin = "auto"),
                   plotOutput("grafico_radial_año", 
                              height = 500, width = 500) |> withSpinner()
               )
           )
      ),
      
      card(full_screen = TRUE,
           
           card_header(
             h4("Temperaturas diarias, por meses")
           ),
           
           div(style = css(margin_top = "-20px", margin_bottom = "-24px"),
               sliderInput("mes_zoom",
                           label = "Meses",
                           min = 1, max = 12,
                           value = c(1, 4), 
                           width = "100%")
           ),
           
           em("Seleccione un rango de meses para acotar la visualización a una temporada del año específica."),
           
           div(style = css(min_width = "600px"),
               plotOutput("grafico_lineal_mes") |> withSpinner()
           )
      )
    )
  ),
  
  ## firma ----
  div(style = "padding: 28px; font-size: 90%;",
      
      h4("Fuentes:"),
      markdown("- Datos de la Dirección General de Aeronáutica Civil subidos a la plataforma [Datos Abiertos del Estado](https://datos.gob.cl/dataset/?q=temperatura)
- [Dirección Meteorológica de Chile](https://climatologia.meteochile.gob.cl)"),
      
      br(),
      
      markdown("Desarrollado en R por [Bastián Olea Herrera.](https://bastianolea.rbind.io)"),
      
      markdown("Puedes explorar mis otras [aplicaciones interactivas sobre datos sociales en mi portafolio.](https://bastianolea.github.io/shiny_apps/)"),
      
      
      markdown("Los datos y el código de fuente de esta app y de la obtención y procesamiento de los datos están [disponibles en el repositorio de GitHub.](https://github.com/bastianolea/temperaturas_chile)")
  )
  
  
  
)

server <- function(input, output) {
  
  
  dato_estacion <- reactive({
    datos |> 
      filter(codigo_nacional == input$estacion)
    
    # browser()
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
                   linewidth = .6, color = color$detalle) +
      # puntos
      geom_point(aes(y = t_max, color = t_max), size = 4) +
      geom_point(aes(y = t_min, color = t_min), size = 4) +
      # textos
      geom_label(aes(label = año, #round(t_max-t_min, 1),
                     y = (t_max+t_min)/2), 
                 label.size = NA,
                 fill = color$fondo, colour = color$texto,
                 size = 3.5, angle = -90, fontface = "bold") +
      geom_text(aes(label = round(t_max, 1), color = t_max,
                    y = t_max + max(t_max)*0.03), size = 3) +
      geom_text(aes(label = round(t_min, 1), color = t_min,
                    y = t_min - max(t_max)*0.03), size = 3) +
      # escalas
      scale_y_continuous(labels = ~paste0(.x, "°"),
                         expand = expansion(c(0.05, 0.05))) +
      scale_x_continuous(breaks = unique(datos_mes$año)) +
      scale_color_gradient(low = color$bajo, high = color$alto) +
      theme_void() +
      theme(text = element_text(colour = color$texto)) +
      guides(color = guide_none()) +
      theme(axis.text.y = element_text(color = color$detalle, size = 9,
                                       margin = margin(l = 7, r = 3)),
            panel.grid.major.y = element_line(linetype = "dashed", color = color$detalle))
  }, res = resolucion, bg = color$fondo)
  
  
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
                   linewidth = .6, color = color$detalle) +
      # puntos
      geom_point(aes(y = t_max, color = t_max), size = 4) +
      geom_point(aes(y = t_min, color = t_min), size = 4) +
      # textos
      geom_label(aes(label = año, #round(t_max-t_min, 1),
                     y = (t_max+t_min)/2), 
                 label.size = NA,
                 fill = color$fondo, color = color$texto,
                 size = 3.5, angle = -90, fontface = "bold") +
      geom_text(aes(label = round(t_max, 1), color = t_max,
                    y = t_max + max(t_max)*0.03), size = 3) +
      geom_text(aes(label = round(t_min, 1), color = t_min,
                    y = t_min - max(t_max)*0.03), size = 3) +
      # escalas
      scale_y_continuous(labels = ~paste0(.x, "°"),
                         # limits = c(0, NA),
                         expand = expansion(c(0.05, 0.05))) +
      scale_x_continuous(breaks = unique(datos_año$año)) +
      scale_color_gradient(low = color$bajo, high = color$alto) +
      theme_void() +
      theme(text = element_text(colour = color$texto)) +
      guides(color = guide_none()) +
      theme(axis.text.y = element_text(color = color$detalle, size = 9,
                                       margin = margin(l = 7, r = 3)),
            panel.grid.major.y = element_line(linetype = "dashed", color = color$detalle))
  }, res = resolucion, bg = color$fondo)
  
  
  
  # máxima anual histórica de una estación ----
  
  output$grafico_promedio_anual_estacion <- renderPlot({
    datos_hist_max <- dato_estacion() |> 
      filter(año >= 1970) |>
      # filter(codigo_nacional == "330021") |>
      mutate(fecha = floor_date(fecha, "month", week_start = 1)) |> 
      group_by(codigo_nacional, fecha, año) |>
      summarize(t_max = max(t_max, na.rm = T),
                t_min = min(t_min, na.rm = T)) |> 
      ungroup()
    
    # promedio anual
    datos_hist_max_prom <- datos_hist_max |> 
      mutate(fecha = floor_date(fecha, "year")) |> 
      group_by(fecha) |> 
      summarize(t_med = mean(t_max),
                t_25 = quantile(t_max, 0.25),
                t_75 = quantile(t_max, 0.75)) |> 
      rename(t_max = t_med)
    
    
    datos_hist_max |> 
      ggplot() +
      aes(x = fecha, y = t_max, color = t_max) +
      # fondo
      # geom_ribbon(data = datos_hist_max_prom,
      #             aes(ymin = t_25,
      #                 ymax = t_75),
      #             color = NA, fill = color$texto,
      #             alpha = .06) +
      # líneas
      geom_line(linewidth = 0.7, alpha = 1) +
      # promedio
      # geom_line(data = datos_hist_max_prom,
      #           linewidth = 2, color = color$fondo, alpha = 1) +
      geom_line(data = datos_hist_max_prom,
                linewidth = 1,
                color = color$alto, 
                alpha = .3) +
      # punto
      # geom_point(data = datos_hist_max_prom |> filter(fecha == max(fecha)),
      #            color = color$fondo, size = 5) +
      # geom_point(data = datos_hist_max_prom |> filter(fecha == max(fecha)),
      #            color = color$alto, size = 4) +
      # escalas
      scale_color_gradient(low = color$bajo, high = color$alto) +
      scale_y_continuous(labels = ~paste0(.x, "°"),
                         limits = c(0, 40)) +
      scale_x_date(breaks = c(ymd(paste(seq(1950, 2024, by = 10), "01-01")),
                              ymd(paste(2024, "01-01"))),
                   #date_breaks = "10 years",
                   date_labels = "%Y", 
                   expand = expansion(c(0, 0.05)),
                   limits = c(min(datos_hist_max$fecha), 
                              max(datos_hist_max$fecha))
      ) +
      guides(color = guide_none()) +
      theme_void() +
      theme(text = element_text(colour = color$texto)) +
      theme(axis.text.x = element_text(face = "bold", angle = -90, margin = margin(b = 10)),
            axis.text.y = element_text(color = color$detalle, size = 9,
                                       margin = margin(l = 7, r = 4)),
            panel.grid.major.y = element_line(linetype = "dashed", color = color$detalle),
            panel.grid.major.x = element_line(color = color$detalle))
    
  }, res = resolucion, bg = color$fondo)
  
  
  # mapa diferencia ----
  datos_mapa <- datos |> 
    filter(año == 2013 | año >= 2016) |>
    # filter(año >= 2013) |> 
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
  
  # unique(datos_mapa$año)
  # datos_mapa |>
  #   filter(año == 2014)
  
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
                 colour = color$detalle, linetype = "dashed", linewidth = 0.3) +
      # puntos
      geom_sf(aes(size = diferencia, color = diferencia), alpha = .8) +
      # escalas
      scale_color_gradient(low = color$bajo, high = color$alto,
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
      theme(text = element_text(colour = color$texto)) +
      guides(color = guide_legend(title = "Diferencia:",position = "bottom", nrow = 1),
             size = guide_legend(title = "Diferencia:",
                                 override.aes = list(alpha = 0.7))) +
      theme(plot.margin = unit(c(0, 3, 0, 2), "mm"),
            strip.text = element_text(face = "bold", size = 12,
                                      margin = margin(b = 4)),
            panel.spacing.x = unit(5, "mm"),
            # panel.grid.major.y = element_line(color = "red"),
            legend.title = element_text(face = "italic", color = color$detalle, size = 9),
            legend.text = element_text(color = color$texto, 
                                       margin = margin(l = 2, r = 8)),
            axis.text.y = element_text(colour = color$texto,
                                       hjust = 1, size = 8, 
                                       margin = margin(r = 2)))
  }, res = resolucion, bg = color$fondo)
  # width = 100*length(unique(datos_mapa$año)))
  
  
  dato_estacion_año <- reactive({
    dato_estacion() |> 
      filter(año >= input$año)
  })
  
  # radial total -----
  output$grafico_radial_total <- renderPlot({
    datos_radial_total <- dato_estacion_año() |> 
      # filter(codigo_nacional == "330021") |>
      mutate(fecha_x = ymd(paste(2024, mes, dia)))
    
    # browser()
    # dev.new()
    datos_radial_total |> 
      ggplot() +
      aes(x = fecha_x, y = t_max) +
      # escala interna y
      annotate("label", label = c("10°", "25°", "35°"), y = c(10, 25, 35), 
               x = as_date("2024-02-15"), label.size = NA,
               size = 3, angle = 90, color = color$detalle, fill = color$fondo) +
      # líneas
      geom_line(aes(group = año, alpha = año, color = t_max), 
                linewidth = 0.2) + 
      # escalas
      scale_color_gradient(low = color$bajo, high = color$alto) +
      scale_x_date(date_breaks = "month", date_labels = "%m",
                   expand = expansion(0)) +
      # temas
      coord_radial(rotate.angle = T, inner.radius = 0.1) +
      theme_void() +
      theme(text = element_text(colour = color$texto)) +
      theme(axis.text.x = element_text(face = "bold", color = color$texto, size = 9),
            panel.grid.major.x = element_line(linetype = "dashed", colour = color$detalle),
            panel.grid.major.y = element_line(colour = color$detalle)) +
      theme(legend.position = "none") +
      theme(plot.margin = unit(rep(-10, 4), "mm"))
  }, res = resolucion, bg = color$fondo)
  
  
  
  
  
  
  # radial mensual ----
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
  
  # datos |> 
  output$grafico_radial_mensual <- renderPlot({
    # browser()
    datos_radial_mes() |> 
      ggplot() +
      aes(x = fecha_x, y = t_max) +
      # escala interna y
      annotate("label", label = c("10°", "25°", "35°"), y = c(10, 25, 35), 
               x = as_date("2024-02-15"), label.size = NA,
               size = 3, angle = 90, color = color$detalle, fill = color$fondo) +
      # líneas
      geom_line(aes(group = año, color = t_max,
                    alpha = actual, linewidth = actual)) + 
      # escalas
      scale_color_gradient(low = color$bajo, high = color$alto) +
      scale_x_date(date_breaks = "month", date_labels = "%m",
                   expand = expansion(0)) +
      scale_y_continuous(breaks = c(10, 25, 35), limits = c(0, 35), 
                         expand = expansion(c(0, 0.1))) +
      scale_alpha_manual(values = c("actual" = 1, "anterior" = 0.2)) +
      scale_linewidth_manual(values = c("actual" = 0.8, "anterior" = 0.2)) +
      # temas
      coord_radial(rotate.angle = T, inner.radius = 0.1) +
      theme_void() +
      theme(text = element_text(colour = color$texto)) +
      theme(axis.text.x = element_text(face = "bold", color = color$texto, size = 9),
            panel.grid.major.x = element_line(linetype = "dashed", colour = color$detalle),
            panel.grid.major.y = element_line(colour = color$detalle)) +
      theme(legend.position = "none") +
      theme(plot.margin = unit(rep(-10, 4), "mm"))
  }, res = resolucion, bg = color$fondo)
  
  # radial ultimo año ----
  datos_radial_año <- reactive({
    dato_estacion_año() |> 
      # filter(codigo_nacional == "330021") |>
      mutate(fecha_x = ymd(paste(2024, mes, dia))) |> 
      mutate(actual = ifelse(año == input$año_zoom, "actual", "anterior"))
  })
  
  # datos |> 
  output$grafico_radial_año <- renderPlot({
    
    datos_radial_año() |> 
      ggplot() +
      aes(x = fecha_x, y = t_max) +
      # escala interna y
      annotate("label", label = c("10°", "25°", "35°"), y = c(10, 25, 35), 
               x = as_date("2024-02-15"), label.size = NA,
               size = 3, angle = 90, color = color$detalle, fill = color$fondo) +
      # líneas
      geom_line(aes(group = año, alpha = actual, linewidth = actual, color = t_max)) + 
      # escalas
      scale_color_gradient(low = color$bajo, high = color$alto) +
      scale_x_date(date_breaks = "month", date_labels = "%m",
                   expand = expansion(0)) +
      scale_alpha_manual(values = c("actual" = 1, "anterior" = 0.4)) +
      scale_linewidth_manual(values = c("actual" = 0.5, "anterior" = 0.2)) +
      # temas
      coord_radial(rotate.angle = T, inner.radius = 0.1) +
      theme_void() +
      theme(text = element_text(colour = color$texto)) +
      theme(axis.text.x = element_text(face = "bold", color = color$texto, size = 9),
            panel.grid.major.x = element_line(linetype = "dashed", colour = color$detalle),
            panel.grid.major.y = element_line(colour = color$detalle)) +
      theme(legend.position = "none") +
      theme(plot.margin = unit(rep(-10, 4), "mm"))
    # theme(plot.background = element_rect(fill = color$fondo, colour = color$fondo),
    #       panel.background = element_rect(fill = color$fondo, colour = color$fondo))
  }, res = resolucion, bg = color$fondo)
  
  
  # lineal mes ----
  output$grafico_lineal_mes <- renderPlot({
    datos_radial_año() |> 
      filter(mes >= input$mes_zoom[1], mes <= input$mes_zoom[2]) |> 
      ggplot() +
      aes(x = fecha_x, y = t_max) +
      # líneas
      geom_line(aes(group = año, alpha = actual, linewidth = actual, color = t_max)) + 
      # escalas
      scale_color_gradient(low = color$bajo, high = color$alto) +
      scale_x_date(date_breaks = "month", date_labels = "%m",
                   expand = expansion(c(0, 0))) +
      scale_y_continuous(labels = ~paste0(.x, "°")) +
      scale_alpha_manual(values = c("actual" = 1, "anterior" = 0.5)) +
      scale_linewidth_manual(values = c("actual" = 0.8, "anterior" = 0.2)) +
      # temas
      # coord_radial(rotate.angle = T, inner.radius = 0.1) +
      theme_void() +
      theme(text = element_text(colour = color$texto)) +
      theme(axis.text.x = element_text(color = color$texto, size = 9,
                                       face = "bold", margin = margin(r = 4, t = 4)),
            axis.text.y = element_text(color = color$detalle, size = 9,
                                       margin = margin(l = 0, r = 4)),
            panel.grid.major.x = element_line(linetype = "dashed", colour = color$detalle),
            panel.grid.major.y = element_line(colour = color$detalle)) +
      theme(legend.position = "none")
  }, res = resolucion, bg = color$fondo)
  
  
}

shinyApp(ui = ui, server = server)
