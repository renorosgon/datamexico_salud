# Fija el directorio de trabajo a la carpeta de datamexico
setwd("~/Desktop/datamexico")

# Instalación y carga de paqueterías
# Instalar - Cargar tidyverse                                                       
if(require(tidyverse) == FALSE){                                                
  install.packages('tidyverse')                                                 
  library(tidyverse)                                                            
}else{                                                                          
  library(tidyverse)                                                            
}

# Instalar - Cargar rvest                                                       
if(require(rvest) == FALSE){                                                
  install.packages('rvest')                                                 
  library(rvest)                                                            
}else{                                                                          
  library(rvest)                                                            
}         

# Instalar - Cargar httr                                                       
if(require(httr) == FALSE){                                                
  install.packages('httr')                                                 
  library(httr)                                                            
}else{                                                                          
  library(httr)                                                            
}    

# Instalar - Cargar readxl                                                       
if(require(readxl) == FALSE){                                                
  install.packages('readxl')                                                 
} 

# Instalar - Cargar lubridate                                                       
if(require(lubridate) == FALSE){                                                
  install.packages('lubridate')                                                 
} 

# Instalar  rjson                                                       
if(require(rjson) == FALSE){                                                
  install.packages('rjson')                                                 
}

# Instalar  janitor                                                       
if(require(janitor) == FALSE){                                                
  install.packages('janitor')                                                 
}

# Producto Interno Burto por Entidad Federativa - Data Mexico/BANXICO
gdp = GET(
  # La url obteniza del Vizbuilder
  url = "https://api.datamexico.org/tesseract/data.jsonrecords?cube=banxico_gdp&drilldowns=Nation%2CState%2CYear&measures=GDP") %>%
  # Lee la data del diccionario json
  read_html() %>% html_text() %>% rjson::fromJSON() %>% .[['data']] %>%
  # Concatena las filas en un tibble y limpia los nombres
  bind_rows() %>% janitor::clean_names() 

# Guarda el output
write_excel_csv(gdp, 'data/pib_estatal.csv')

# Población por lustro - Data Mexico/CONAPO
poblacion = GET(
  # Define la url principal
  url = "https://api.datamexico.org/tesseract/data.jsonrecords?cube=conapo_metro_area_population&drilldowns=State%2CYear&locale=es&measures=Population") %>%
  # Lee la data del diccionario json
  read_html() %>% html_text() %>% rjson::fromJSON() %>% .[['data']] %>%
  # Concatena las filas en un tibble y limpia los nombres
  bind_rows() %>% janitor::clean_names() 

# Guarda el output
write_excel_csv(poblacion, 'data/poblacion_conteos.csv')

# Proyecciones de la Población  - Data Mexico/CONAPO
proyecciones_poblacion = GET(
  # Define la url principal
  url = "https://api.datamexico.org/tesseract/data.jsonrecords?cube=population_projection&drilldowns=State%2CYear&measures=Projected+Population") %>%
  # Lee la data del diccionario json
  read_html() %>% html_text() %>% rjson::fromJSON() %>% .[['data']] %>%
  # Concatena las filas en un tibble y limpia los nombres
  bind_rows() %>% janitor::clean_names() 

# Guarda el output
write_excel_csv(proyecciones_poblacion, 'data/poblacion_proyecciones.csv')

# Participaciones a Entidades Federativas y Municipios - Data Mexico
participaciones = GET(
  # Define la url principal
  url = "https://api.datamexico.org/tesseract/data.jsonrecords?Expenses+Type=9&cube=budget_transparency&drilldowns=Functional+Group%2CFunction%2CSub+Function%2CDepartment%2CExpenses+Type%2CState%2CYear&locale=es&measures=Amount+Approved%2CAmount+Executed") %>%
  # Lee la data del diccionario json
  read_html() %>% html_text() %>% rjson::fromJSON() %>% .[['data']] %>%
  # Concatena las filas en un tibble y limpia los nombres
  bind_rows() %>% janitor::clean_names() 

# Guarda el output
write_excel_csv(participaciones, 'data/participaciones_estados_municipios.csv')

# Morbilidad - Data Mexico
morbilidad = c()

# Itera para los años disponibles
for(anio in 2012:2018){
  morbilidad = GET(
    # Define la url principal
    url = paste0("https://api.datamexico.org/tesseract/data.jsonrecords?Year=",anio,"&cube=dgis_emergency&drilldowns=Category%2CYear%2CState&locale=en&measures=Total")) %>% 
    # Lee la data del diccionario json
    read_html() %>% html_text() %>% rjson::fromJSON() %>% .[['data']] %>%
    # Concatena las filas en un tibble, limpia los nombres y concatena en el tibble
    bind_rows() %>% janitor::clean_names() %>% bind_rows(morbilidad )
}

# Actualiza las claves CIE10 en grupos para homologar con INEGI
morbilidad = morbilidad %>% 
  mutate(cve_cie = case_when(
    str_detect(category_id, "A|B") ~ "A00-B99",
    str_detect(category_id, "C|D+[0-4]") ~ "C00-D48",
    str_detect(category_id, "D+[5-9]") ~ "D50-D89",
    str_detect(category_id, "E") ~ "E00-E90",
    str_detect(category_id, "F") ~ "F00-F99",
    str_detect(category_id, "G") ~ "G00-G99",
    str_detect(category_id, "H") ~ "H00-H59",
    str_detect(category_id, "I") ~ "I00-I99",
    str_detect(category_id, "J") ~ "J00-J99",
    str_detect(category_id, "K") ~ "K00-K93",
    str_detect(category_id, "L") ~ "L00-L99",
    str_detect(category_id, "M") ~ "M00-M99",
    str_detect(category_id, "N") ~ "N00-N99",
    str_detect(category_id, "O") ~ "O00-O99",
    str_detect(category_id, "P") ~ "P00-P96",
    str_detect(category_id, "Q") ~ "Q00-Q99",
    str_detect(category_id, "R") ~ "R00-R99",
    str_detect(category_id, "S|T") ~ "S00-T99",
    str_detect(category_id, "U") ~ "U00-U49",
    str_detect(category_id, "V|W|X|Y") ~ "V01-Y98",
    str_detect(category_id, "Z") ~ "Z00-Z99"
  )
  ) %>% 
  with_groups(
    # Agrupa por año, entidad y grupo CIE10
    .groups = c(year, state_id, state, cve_cie),
    # Agrega las variables
    summarise,
    # Obteniendo el total por grupo
    morbilidad = sum(total, na.rm = T)
  )

# Guarda el output
write_excel_csv(morbilidad, 'data/morbilidad.csv')

# Capacidad Hospilataria - Data Mexico
capacidad = GET(
  # Define la url principal
  url = "https://api.datamexico.org/tesseract/data.jsonrecords?cube=health_resources&drilldowns=State%2CYear%2CResources+Categories&measures=Total") %>% 
  # Lee la data del diccionario json
  read_html() %>% html_text() %>% rjson::fromJSON() %>% .[['data']] %>%
  # Concatena las filas en un tibble y limpia los nombres
  bind_rows() %>% janitor::clean_names() 

# Guarda el output
write_excel_csv(capacidad, 'data/capacidad_hospitalaria.csv')

# Precios del Petróleo (Mezcla Maya) - US Energy Information Administration
precios_petroleo = GET(
  # Define la url de la consulta de precios
  url = "https://www.eia.gov/dnav/pet/hist/LeafHandler.ashx?n=PET&s=IMX2810004&f=M") %>%
  # Lee el documento html
  read_html() %>% 
  # Apunta al nodo con la tabla de precios
  html_nodes(xpath = '//table[@class="FloatTitle"]') %>% 
  # Lee la tabla de precios
  html_table() %>% .[[1]] %>% 
  # Filtramos valores NA
  filter(!is.na(Year)) %>% 
  # Agrupamos por mes y año
  gather(month, price, -Year) %>% 
  # Transformamos la fecha y el precio
  transmute(
    date = lubridate::mdy(paste(month, '01,', Year)),
    price = price
  )

# Guarda el output
write_excel_csv(precios_petroleo, 'data/precios_petroleo_mezcla_maya.csv')

# Los datos de mortalidad fueron obtenidos de INEGI al no estar aún disponibles en Data Mexico
# https://www.inegi.org.mx/sistemas/olap/Proyectos/bd/continuas/mortalidad/MortalidadGeneral.asp
mortalidad = readxl::read_excel('data/mortalidad_inegi.xlsx') %>% 
  # Estructora en formato largo (long)
  gather(CIE10, mortalidad, -c(cve_ent, ent, year)) %>% 
  # Filtra Totales
  filter(!is.na(mortalidad),!(CIE10 %in% c('Total','CIE-10/2'))) %>% 
  # Crea y actualiza variables
  mutate(
    # Grupos por clave CIE10
    cve_cie = str_extract(CIE10, '[A-Z][0-9]+-[A-Z][0-9]+'),
    # Transforma las variables a numéricas
    mortalidad = as.numeric(str_remove_all(mortalidad,',')),
    cve_ent = as.numeric(cve_ent),
    year = as.numeric(year),
    # Homologa el nombre de la entidad
    ent = if_else(ent == 'México','Estado de México', ent)) %>% 
  rename(state_id = cve_ent, state = ent)

# Construye las tasas de letalidad
estadisticas_salud = mortalidad %>% 
  # Concatena mortalidad y morbilidad
  inner_join(morbilidad, by = c('state_id', 'state','year','cve_cie')) %>% 
  # Se asume letalidad como la proporción de personas fallecidas dado que estaban enfermas
  mutate(letalidad = mortalidad/morbilidad)

# Guarda el output
write_excel_csv(estadisticas_salud,'data/estadisticas_salud.csv')






