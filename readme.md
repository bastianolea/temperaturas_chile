# Datos de temperaturas extremas en Chile

[Visualizador de datos de temperaturas extremas en el país.]((https://bastianoleah.shinyapps.io/temperaturas_chile/)) Permite consultar rápidamente datos históricos para obtener gráficos que revelan los cambios en temperaturas a través del tiempo en nuestro país. El objetivo es visibilizar los efectos del cambio climático por medio de los datos.

En esta plataforma puedes visualizar datos históricos sobre temperaturas extremas del país, cuyo objetivo es visibilizar los efectos del cambio climático por medio de los datos.

Los datos se obtienen desde el Portal de Datos Abiertos del Estado de Chile mediante web scrapping. El proyecto unifica todas las fuentes de datos separadas en una sola base de datos de temperaturas, los cuales son utilizados en la plataforma de visualización interactiva. 

Los datos son obtenidos desde fuentes oficiales, y abarcan aproximadamente desde 1970 a 2024.

Este repositorio obtiene datos desde el [Portal de Datos Abiertos del Estado de Chile](https://datos.gob.cl/dataset/?q=temperatura) mediante web scrapping, unifica todas las fuentes de datos separadas en una sola base de datos de temperaturas, y produce visualizaciones y una [plataforma de visualización interactiva.](https://bastianoleah.shinyapps.io/temperaturas_chile/)


[Accede al visualizador de datos por este enlace.](https://bastianoleah.shinyapps.io/temperaturas_chile/)

El producto principal de este código es una base de datos de temperaturas extremas en Chile, por estación meteorológica y por día, desde 1970 a 2024 [disponible convenientemente en un solo archivo `.parquet`](https://github.com/bastianolea/temperaturas_chile/raw/main/temperaturas_chile/temperaturas.parquet) (también disponible [en `.rds`](https://github.com/bastianolea/temperaturas_chile/raw/main/datos/procesados/temperaturas_chile_unificadas.rds) y [en formato `.csv`](https://github.com/bastianolea/temperaturas_chile/raw/main/datos/procesados/temperaturas_chile_unificadas.csv))


## Obtención de los datos

Los datos son obtenidos de forma semi-automática usando técnicas de web scrapping con el paquete {RSelenium}. Esto debido a que el portal de datos abiertos no tiene una buena interfaz de usuario (no permite abrir resultados en nuevas pestañas, no permite copiar los enlaces de las fuentes de datos, y hay que actualizar las páginas entre 1 a 8 veces para que muestren los resultados). El script `obtener_temperaturas.R` simplifica tres tareas para obtener los datos: realizar una búsqueda y ampliar la cantidad de resultados mostrados en la página, entrar a cada uno de los enlaces del resultado de la búsqueda, y dentro de los conjuntos de datos, descargar todos los archivos con un solo comando. Esta ahorra muchísimo tiempo, considerando que los datos de temperatura vienen separados por semestre y año, lo que significa que hay que entrar a aproximadamente 16 conjuntos de datos distintos, y dentro de estos conjuntos de datos hay que descargar aproximadamente 6 archivos separados, mientras la plataforma dificulta abrir estos resultados por pestañas.

## Datos
- [`temperaturas_chile.csv`](datos/procesados/temperaturas_chile.csv): resultado de descargar todos los datos existentes en la plataforma y unificarlos en una sola tabla de datos. Abarca los años de 2013, y 2016 a 2024 (este último desde enero a mayo). Contiene columnas con el código de la estación, latitud y longitud de la estación, fecha, temperatura máxima, temperatura mínima y altura. [También disponible](datos/procesados/temperaturas_chile.rds) en formato `.rds` para leer desde R.
- [`temperaturas_historicas_chile.csv`](datos/procesados/temperaturas_historicas_chile.csv): resultados de la obtención de datos históricos de temperaturas extremas desde el sitio de la [Dirección Meteorológica de Chile](https://climatologia.meteochile.gob.cl/application/historico/temperaturaHistoricaMes/180005/1). La obtención de los datos se realiza en el script `obtener_historicas.R`. Contienen las temperaturas máximas, mínimas y medias de cada mes, por año y por estación meteorológica, desde aproximadamente 1955 a 2024.
- [`temperaturas_chile_unificadas.csv`](datos/procesados/temperaturas_chile_unificadas.csv): los dos conjuntos de datos anteriores pero unidos en el script `unificar_datos.R`, resultando una base con datos de temperaturas mínimas y máximas por año y estación, desde 1950 hasta 2024.
- [`estaciones_meteorologicas_chile.csv`](datos/estaciones_meteorologicas_chile.csv): listado de estaciones meteorológicas y sus características, obtenido con web scraping en el script `obtener_estaciones.R`, desde el sitio web de la [Dirección General De Aeronáutica Civil](https://climatologia.meteochile.gob.cl/application/informacion/buscadorEstaciones)


## Fuentes
- Datos de la Dirección General de Aeronáutica Civil subidos a la plataforma de [Datos Abiertos del Estado](https://datos.gob.cl/dataset/?q=temperatura)
- [Dirección Meteorológica de Chile](https://climatologia.meteochile.gob.cl)

## Visualizador de datos

[Accede al visualizador de datos por este enlace.](https://bastianoleah.shinyapps.io/temperaturas_chile/)

![](pantallazos/temperaturas_chile_a.jpeg)

![](pantallazos/temperaturas_chile_b.jpeg)