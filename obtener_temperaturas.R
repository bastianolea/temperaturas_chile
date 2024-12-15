library(RSelenium)
library(dplyr)
library(stringr)
library(purrr)
library(tidyr)
library(stringr)
library(glue)

# crear el navegador
driver <- rsDriver(port = 4567L, browser = "firefox", chromever = NULL)
remote_driver <- driver[["client"]]

# navegar a la página
remote_driver$navigate("https://datos.gob.cl/dataset/?q=temperatura+2013")

remote_driver$refresh()

# # aumentar resultados por página
# remote_driver$findElement(using = 'css selector', 
#                           ".ant-pagination-options-size-changer > div:nth-child(1) > span:nth-child(2)")$
#   clickElement()
# 
# # elegir 100 resultados
# remote_driver$findElement(using = 'css selector', 
#                           "#rc_select_1_list_3 > div:nth-child(1)")$
#   clickElement()
# 





# elegir el botón para entrar al conjunto de datos
# el primero es el 2
remote_driver$findElement(using = 'css selector', 
                          "div.card:nth-child(2) > div:nth-child(1) > div:nth-child(2) > button:nth-child(1)")$
  clickElement()

remote_driver$refresh()


# # descargar los datos (del 2 al 7 en section.px-4:nth-child(7))
walk(6:7, ~{
  message(.x)
  remote_driver$findElement(using = 'css selector', 
                            glue("section.px-4:nth-child({.x}) > div:nth-child(1) > div:nth-child(2) > button:nth-child(2) > svg:nth-child(1)"))$
    clickElement()
  Sys.sleep(1)
})

# volver (solo si vas en el segundo botón)
remote_driver$goBack()

# repetir

