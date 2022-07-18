# Fija el directorio de trabajo a la carpeta de datamexico
setwd("~/Desktop/datamexico")

# Instalar - Cargar tidyverse                                                       
if(require(tidyverse) == FALSE){                                                
  install.packages('tidyverse')                                                 
  library(tidyverse)                                                            
}else{                                                                          
  library(tidyverse)                                                            
}

# Instalar - Cargar p2distance  
# Nota: el CRAN ya solo existe en archivo. Posiblemente haya que instalarlo con devtools
if(require(p2distance) == FALSE){                                                
  install.packages('p2distance')                                                 
  library(p2distance)                                                            
}else{                                                                          
  library(p2distance)                                                            
}

# En caso de aún no tener los datos.
source('scripts/get_data.R')

# Este código calcula los índices de distancia p2 de pena para los fenómenos de interés
# Definimos una función para extraer el índice
calcula_p2 = function(df,funcion_referencia = 'min'){
  # Crea el vector de referencia (peor escenario)
  vector_de_referencia = df %>% 
    filter(year %in% 2012:2018) %>% 
    # Seleccia las variables de interés
    select(-c(state_id, state, year)) %>% 
    # Obtiene el valor de referencia
    summarise_all(funcion_referencia)
  
  distancia_p2 = c()
  # Calcula la distancia P2 de pena para cada año
  for(anio in 2012:2018){
    distancia_p2 = df %>% 
      # Filtra el año
      filter(year == anio) %>% 
      # Selecciona las variables de interés
      select(-c(state_id, state, year)) %>% 
      # Transforma a matriz
      as.matrix() %>% 
      scale() %>% 
      # # Calcula la distancia P2 de pena
      p2distance(reference_vector = vector_de_referencia) %>% 
      pluck('p2distance') %>% .[,1] %>% 
      as_tibble() %>% 
      bind_rows(distancia_p2)
  }
  
  # regresa el indicador
  return(distancia_p2 %>% pull(value))
}

# Personal por cada 100 mil habitantes
personal = capacidad %>% 
  # Concatena la capacidad hospitalaria con las proyecciones de población
  left_join(proyecciones_poblacion, by = c("state_id", "state", "year")) %>% 
  # Calcula la disponibilidad por cada 100 mil habitantes
  mutate(total = (total/projected_population) * 100000) %>% 
  # Selecciona las variables de interés
  select(state_id, state, year, resources_categories, total) %>%
  # Filtra los elementos referentes a personal
  filter(str_detect(resources_categories, "[Pp]ersonal|[Mm]édicos")) %>%
  # Expande por tipo de personal
  spread(resources_categories, total, fill = 0) %>% 
  # Limpia los nombres
  janitor::clean_names() %>% 
  # Ordena por año y entidad
  arrange(year, state_id) 

# Guarda el output 
write_excel_csv(personal, 'data/personal_por_mil_habitantes.csv')

# Infraestructura por cada 100 mil habitantes
infraestructura = capacidad %>% 
  # Concatena la capacidad hospitalaria con las proyecciones de población
  left_join(proyecciones_poblacion, by = c("state_id", "state", "year")) %>% 
  # Calcula la disponibilidad por cada 100 mil habitantes
  mutate(total = (total/projected_population) * 100000) %>% 
  # Selecciona las variables de interés
  select(state_id, state, year, resources_categories, total) %>%
  # Filtra los elementos referentes a infraestructura
  filter(!(str_detect(resources_categories, "[Pp]ersonal|[Mm]édicos"))) %>%
  # Expande por tipo de infraestructura
  spread(resources_categories, total, fill = 0) %>% 
  # Limpia los nombres
  janitor::clean_names() %>% 
  # Ordena por año y entidad
  arrange(year, state_id)  %>% 
  # Homologa los años
  filter(year %in% 2012:2018) 

# Guarda el output 
write_excel_csv(infraestructura, 'data/infraestructura_por_mil_habitantes.csv')

# Estadísticas para la salud
estadisticas_salud = estadisticas_salud  %>% 
  # Concatena la capacidad hospitalaria con las proyecciones de población
  left_join(proyecciones_poblacion, by = c("state_id", "state", "year")) %>% 
  filter(cve_cie != "V01-Y98")

# Mortalidad por cada 100 mil habitantes
mortalidad = estadisticas_salud %>% 
  # Calcula la disponibilidad por cada 100 mil habitantes
  mutate(total = (mortalidad/projected_population) * 100000) %>% 
  # Selecciona las variables de interés
  select(state_id, state, year, CIE10, total) %>%
  # Expande por tipo de letalidad
  spread(CIE10, total, fill = 0) %>% 
  # Limpia los nombres
  janitor::clean_names() %>% 
  # Ordena por año y entidad
  arrange(year, state_id)  

# Guarda el output 
write_excel_csv(mortalidad, 'data/mortalidad_por_mil_habitantes.csv')

# Morbilidad
morbilidad = estadisticas_salud %>% 
  # Calcula la disponibilidad por cada 100 mil habitantes
  mutate(total = (morbilidad/projected_population) * 100000) %>% 
  # Selecciona las variables de interés
  select(state_id, state, year, CIE10, total) %>%
  # Expande por tipo de morbilidad
  spread(CIE10, total, fill = 0) %>% 
  # Limpia los nombres
  janitor::clean_names() %>% 
  # Ordena por año y entidad
  arrange(year, state_id) 

# Guarda el output 
write_excel_csv(morbilidad, 'data/morbilidad_por_mil_habitantes.csv')

# Letalidad 
letalidad = estadisticas_salud %>% 
  # Calcula la disponibilidad por cada 100 mil habitantes
  mutate(total = letalidad) %>% 
  # Selecciona las variables de interés
  select(state_id, state, year, CIE10, total) %>%
  # Expande por tipo de letalidad
  spread(CIE10, total, fill = 0) %>% 
  # Limpia los nombres
  janitor::clean_names() %>% 
  # Ordena por año y entidad
  arrange(year, state_id) 

# Guarda el output 
write_excel_csv(letalidad, 'data/letalidad_codigo_cie10.csv')

names(letalidad)[4:20] = paste0(names(letalidad)[4:20],'_letalidad')

# Concatenamos los datos
data_final = gdp %>% 
  inner_join(proyecciones_poblacion, by = c("state_id", "state", "year")) %>% 
  inner_join(personal, by = c("state_id", "state", "year")) %>% 
  inner_join(infraestructura, by = c("state_id", "state", "year")) %>% 
  inner_join(mortalidad, by = c("state_id", "state", "year")) %>% 
  inner_join(morbilidad, by = c("state_id", "state", "year"), 
             suffix = c('_mortalidad','morbilidad')) %>% 
  inner_join(letalidad, by = c("state_id", "state", "year")) %>% 
  arrange(year, state_id) %>% 
  mutate(
    personal_p2 = calcula_p2(personal),
    infraestructura_p2 = calcula_p2(infraestructura),
    mortalidad_p2 = calcula_p2(mortalidad, funcion_referencia = 'max'),
    morbilidad_p2 = calcula_p2(morbilidad, funcion_referencia ='max'),
    letalidad_p2 = calcula_p2(letalidad, funcion_referencia = 'max')
  )
  
# Guarda el output
write_excel_csv(data_final,'data/distancias_p2.csv')
