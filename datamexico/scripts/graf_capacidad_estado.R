#rm(list=ls())
library(tidyverse)
library(rjson)
library(rvest)
library(httr)
library(p2distance)
library(readxl)
library(ggthemes)
library(ggcorrplot)


# Queries -----------------------------------------------------------------
##Query para obtener población
poblacion <- read_excel("Poblacion_01.xlsx", skip =1) %>% 
  rename(state = `Entidad federativa`,
         poblacion = Total) %>% 
  mutate(state = case_when(
    state %in% "México" ~ "Estado de México",
    TRUE ~ state
  )) %>% 
  filter(!is.na(state))

##Query para obtener participaciones
participaciones = GET(
  # Define la url principal
  url = "https://api.datamexico.org/tesseract/data.jsonrecords?Expenses+Type=9&cube=budget_transparency&drilldowns=Functional+Group%2CFunction%2CSub+Function%2CDepartment%2CExpenses+Type%2CState%2CYear&locale=es&measures=Amount+Approved%2CAmount+Executed") %>%
  # Lee la data del diccionario json
  read_html() %>% html_text() %>% rjson::fromJSON() %>% .[['data']] %>%
  # Concatena las filas en un tibble y limpia los nombres
  bind_rows() %>% janitor::clean_names()  %>% 
  left_join(poblacion) %>% 
  mutate(pps = (amount_approved/poblacion)*100) %>% 
  filter(year ==2020,
         !str_detect(state, "No"))

##Hago el query para obtener capacidad
capacidad = GET(
  # Define la url principal
  url = "https://api.datamexico.org/tesseract/data.jsonrecords?cube=health_resources&drilldowns=State%2CYear%2CResources+Categories&measures=Total"
) %>% read_html() %>% html_text() %>% rjson::fromJSON() %>% .[['data']] %>% 
  bind_rows() %>% janitor::clean_names()

covid = GET(
  # Define la url principal
  url = "https://api.datamexico.org/tesseract/data.jsonrecords?cube=gobmx_covid&drilldowns=Is+Dead%2CCovid+Result%2CState%2CHealth+Institution+Attended&locale=es&measures=Cases"
) %>% read_html() %>% html_text() %>% rjson::fromJSON() %>% .[['data']] %>% 
  bind_rows() %>% janitor::clean_names()%>% 
  mutate(institution = case_when(
    str_detect(health_institution_attended, "ISSSTE") ~ "ISSSTE",
    str_detect(health_institution_attended, "No") ~ "No",
    str_detect(health_institution_attended, "IMSS") ~ "IMSS",
    str_detect(health_institution_attended, "SEMAR") ~ "Pemex, Defensa o Marina",
    str_detect(health_institution_attended, "SEDENA") ~ "Pemex, Defensa o Marina",
    str_detect(health_institution_attended, "PEMEX") ~ "Pemex, Defensa o Marina",
    str_detect(health_institution_attended, "Privada") ~ "SMP",
    str_detect(health_institution_attended, "SSA") ~ "SSA",
    str_detect(health_institution_attended, "Otra") ~ "No",
    TRUE ~ "No"
  )) %>% 
  filter(!institution%in% "No")


capacidad_covid = GET(
  # Define la url principal
  url = "https://api.datamexico.org/tesseract/data.jsonrecords?cube=health_establishments&drilldowns=Institution%2CState&measures=Clinics%2CBeds%2CEstablishments"
) %>% read_html() %>% html_text() %>% rjson::fromJSON() %>% .[['data']] %>% 
  bind_rows() %>% janitor::clean_names()%>% 
  mutate(institution = case_when(
    str_detect(institution, "ISSSTE") ~ "ISSSTE",
    str_detect(institution, "No") ~ "No",
    str_detect(institution, "IMSS") ~ "IMSS",
    str_detect(institution, "SEMAR") ~ "Pemex, Defensa o Marina",
    str_detect(institution, "SEDENA") ~ "Pemex, Defensa o Marina",
    str_detect(institution, "PEMEX") ~ "Pemex, Defensa o Marina",
    str_detect(institution, "Seguro Popular o para una Nueva Generación \\(Siglo XXI\\)") ~ "SSA",
    str_detect(institution, "Otra") ~ "No",
    TRUE ~ institution
  )) 



# Limpieza de bases y calculos de P2 --------------------------------------------
##Limpio base
datos <- capacidad %>% 
  left_join(poblacion) %>% ##Uno con poblacion
  mutate(total = (total/poblacion)*100000) %>%  ##Obtengo las metricas por 100,000 hab
  select(estado = state, ano = year, recurso = "resources_categories", total) %>% 
  filter(ano == 2020)  ##Filtro para 2020

## Me quedo con los datos relacionados a personal o medicos
personal <- datos %>% 
  filter(str_detect(recurso, "[Pp]ersonal")|
           str_detect(recurso, "[Mm]édicos")) %>% 
  select(!ano) %>% 
  pivot_wider(names_from = "recurso", values_from = "total") %>% 
  replace(is.na(.), 0) 

##Genero el vector de referencia para efectuar indice de Pena
rv <- as_tibble(sapply(personal, min, na.rm = T)) %>% 
  filter(!value %in% "1",
         !value %in% "Aguascalientes") %>% 
  mutate(value = as.numeric(value))

##Creo la matriz
matriz <- as.matrix(personal[,2:length(personal)])

##Calculo de la distancia de Pena
p2 <- p2distance(matriz, 
                 reference_vector = rv)

p2d <- p2$p2distance
name <- colnames(p2d)

##Genero el tibble de personal con los resultados
personal <- as.data.frame(cbind(personal[, "estado"],p2[["p2distance"]])) %>% 
  dplyr::rename(P2 =  name,
                Estado = estado) %>% 
  arrange(desc(P2)) %>% 
  rename(state= Estado,
         Personal = P2)



##Repito el proceso pero para recursos
recursos <- datos %>% 
  filter(!str_detect(recurso, "[Pp]ersonal"),
         !str_detect(recurso, "[Mm]édicos"))%>% 
  select(!ano) %>% 
  pivot_wider(names_from = "recurso", values_from = "total")

##Vector de referencia
rv <- as_tibble(sapply(recursos, min, na.rm = T)) %>% 
  filter(!value %in% "1",
         !value %in% "Aguascalientes") %>% 
  mutate(value = as.numeric(value))

##matriz de datos
matriz <- as.matrix(recursos[,2:length(recursos)])

##Calculo de la distancia de Pena
p2 <- p2distance(matriz, 
                 reference_vector = rv)

p2d <- p2$p2distance
name <- colnames(p2d)

##Guardo los resultados en recursos
recursos <- as.data.frame(cbind(recursos[, "estado"],p2[["p2distance"]])) %>% 
  dplyr::rename(P2 =  name,
                Estado = estado) %>% 
  arrange(desc(P2))    %>% 
  rename(state= Estado,
         Recursos = P2)


## Repito para capacidad
capacidad_estado <- capacidad_covid %>% 
  group_by(state) %>%  ##Agrupo por estado
  summarise(across(clinics:establishments,sum)) %>%  ##Sumo los establecimientos
  left_join(select(poblacion, state, poblacion), by = "state") %>% 
  mutate(across(clinics:establishments, ~ (./poblacion)*100000)) %>% ## estandarizo a 100,000 hab
  select(!poblacion)

##Hago el p2 de capacidad
##Vector de referencia
rv <- as_tibble(sapply(capacidad_estado, min, na.rm = T)) %>% 
  filter(!value %in% "1",
         !value %in% "Aguascalientes") %>% 
  mutate(value = as.numeric(value))

##Matriz de datos
matriz <- as.matrix(capacidad_estado[,2:length(capacidad_estado)])

#calculo de la distancia de Pena
p2 <- p2distance(matriz, 
                 reference_vector = rv)

p2d <- p2$p2distance
name <- colnames(p2d)

##Guardo los resultados 
capacidad <- as.data.frame(cbind(capacidad_estado[, "state"],p2[["p2distance"]])) %>% 
  dplyr::rename(P2_capacidad =  name)%>% 
  rename(Capacidad= P2_capacidad)


##Calculo la letalidad por estado
letalidad_estado <- covid %>% 
  group_by(state) %>% 
  mutate(muertos = is_dead_id*cases) %>% 
  summarise(total_casos = sum(cases),
            total_muertes = sum(muertos)) %>% 
  mutate(
    letalidad = total_muertes/total_casos
  )


##Creo la base que servirá para el gráfico
base <- personal %>% 
  left_join(recursos, by = "state") %>% 
  left_join(capacidad) %>% 
  left_join(letalidad_estado)

## Grafico de puntos. 
ggplot(base, aes(x = reorder(state, letalidad))) +
  geom_segment( aes(xend= reorder(state,letalidad), y=0, yend=16), color="grey90") +
  geom_point( aes( y = Recursos,color="Infraestructura"), size=3, alpha = 0.8) +
  geom_point( aes( y = Capacidad, color="Capacidad"), size=3, alpha = 0.8) +
  geom_point( aes( y = Personal,color="Personal"), size=3, alpha = 0.8) +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "right",
    axis.text.y =  element_blank(),
    axis.title.y = element_blank(),
    title = element_text(size = 8)
  ) +
  labs( 
    title = "Índices de capacidad de \n atención médica por estado",
    x = "Estado",
    y = "Resultado en el Índice") +
  scale_color_manual(
    name= "Índice",
    values= c("Personal" = "#003049","Capacidad" = "#f77f00","Infraestructura" = "#fcbf49")
  ) 
ggsave("capacidad_covid_estado.png", last_plot(), units = "mm", height = 200, width = 100)



##Correlograma
# Por estado
##Genero la base necesaria para la correlacion
base <- letalidad_estado %>% 
  full_join(capacidad, by = "state")%>%  
  full_join(participaciones) %>% 
  full_join(personal) %>% 
  full_join(recursos, by = "state") %>% 
  select(Letalidad =letalidad, Capacidad,Participaciones = pps, Personal, 
         Infraestructura = Recursos) #Selecciono variables numéricas de interés


##Genero la correlación
base.cor <- cor(base, method = c("spearman"))


##Hago el gráfico de correlación
ggcorrplot(base.cor,
           outline.col = "white",
           ggtheme = ggplot2::theme_gray,
           colors = c("#d62828", "white", "#003049"), lab = T,
           lab_size = 5)
