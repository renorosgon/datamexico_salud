# Instalar - Cargar tidyverse                                                       
if(require(tidyverse) == FALSE){                                                
  install.packages('tidyverse')                                                 
  library(tidyverse)                                                            
}else{                                                                          
  library(tidyverse)                                                            
}

# En caso de aún no tener los datos y las distancias calcuadas.
source('scripts/get_p2.R')

# Serie temporal del indice de letalidad
letalidad_graph = data_final %>% 
  ggplot(aes(x = year, y = letalidad_p2)) +
  geom_line(aes(group = state),col = 'gray', lwd = 0.3) + 
  stat_summary(geom = "line", fun = mean) +
  labs(title = 'Letalidad') +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 5),
        axis.text.y = element_text(size = 5),
        plot.title = element_text(size = 7)
  )

letalidad_graph

ggsave('graphs/indice_letalidad.png',
       width = 1500,
       height = 500,
       units = 'px',
       dpi = 300
)


# Serie temporal del indice de morbilidad
morbilidad_graph = data_final %>% 
  ggplot(aes(x = year, y = morbilidad_p2)) +
  geom_line(aes(group = state),col = 'gray', lwd = 0.3) + 
  stat_summary(geom = "line", fun = mean) +
  labs(title = 'Morbilidad') +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 5),
        axis.text.y = element_text(size = 5),
        plot.title = element_text(size = 7)
  )

morbilidad_graph

ggsave('graphs/indice_morbilidad.png',
       width = 1500,
       height = 500,
       units = 'px',
       dpi = 300
)

mortalidad_graph = data_final %>% 
  ggplot(aes(x = year, y = mortalidad_p2)) +
  geom_line(aes(group = state),col = 'gray', lwd = 0.3) + 
  stat_summary(geom = "line", fun = mean) +
  labs(title = 'Mortalidad') +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 5),
        axis.text.y = element_text(size = 5),
        plot.title = element_text(size = 7)
  )

mortalidad_graph

ggsave('graphs/indice_mortalidad.png',
       width = 1500,
       height = 500,
       units = 'px',
       dpi = 300
)

# Serie temporal del indice de personal
personal_graph = data_final %>% 
  ggplot(aes(x = year, y = personal_p2)) +
  geom_line(aes(group = state),col = 'gray', lwd = 0.3) + 
  stat_summary(geom = "line", fun = mean) +
  labs(title = 'Personal') +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 5),
        axis.text.y = element_text(size = 5),
        plot.title = element_text(size = 7)
  )

personal_graph

ggsave('graphs/indice_personal.png',
       width = 1500,
       height = 500,
       units = 'px',
       dpi = 300
)

# Serie temporal del indice de infraestructura
infraestructura_graph = data_final %>% 
  ggplot(aes(x = year, y = infraestructura_p2)) +
  geom_line(aes(group = state),col = 'gray', lwd = 0.3) + 
  stat_summary(geom = "line", fun = mean) +
  labs(title = 'Infraestructura') +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 5),
        axis.text.y = element_text(size = 5),
        plot.title = element_text(size = 7)
  )

infraestructura_graph

ggsave('graphs/indice_infraestructura.png',
       width = 1500,
       height = 500,
       units = 'px',
       dpi = 300
)

# Correlacion por estado
con_participaciones = data_final %>% 
  inner_join(participaciones, by = c("state_id", "state", "year")) %>% 
  mutate(participaciones = amount_executed/projected_population)


corr_matrices = function(anio){
  cor_df = con_participaciones %>%
    filter(state == anio) %>% 
    select(personal_p2,infraestructura_p2,participaciones) %>% 
    cor() %>% 
    as_tibble(rownames = 'indice_from') %>% 
    gather(indice_to, correlacion, -indice_from) %>% 
    mutate(year = anio)
  return(cor_df)
}

lapply(unique(personal$state),corr_matrices) %>% 
  bind_rows() %>% 
  filter(indice_to =='participaciones', 
         correlacion<1)%>% 
  mutate(indice_from = factor(str_to_title(str_remove(indice_from,'_[a-z0-9]+')),
                              levels = c('Personal','Infraestructura','Participaciones'))) %>% 
  ggplot(aes(y = year, x = correlacion, fill = correlacion > 0))+
  geom_col(show.legend = F) + 
  labs(x = 'Correlación de Pearson') +
  facet_wrap(~indice_from, scales = 'free_x') +
  theme_bw() +
  theme(axis.title.x = element_text(size = 5),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 5),
        axis.text.y = element_text(size = 5),
        strip.text =  element_text(size = 5)
  )

ggsave('graphs/correlaciones.png',
       width = 1500,
       height = 500,
       units = 'px',
       dpi = 300
)

