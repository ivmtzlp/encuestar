encuesta_demo$muestra$diseno$variables|>
  encuestar:::analizar_blackbox_1d(vars = c('stimuli_1','stimuli_2','stimuli_3',
                                            'stimuli_4','stimuli_5','stimuli_6','stimuli_7'),
                                   stimuli = 'voto_pr_24')


library(dplyr)


encuesta_demo$muestra$diseno$variables|>
  select(all_of(c('stimuli_1',,'stimuli_2','stimuli_3',
                  'stimuli_4','stimuli_5','stimuli_6','stimuli_7')))



codetools::findGlobals(encuestar:::analisis_correspondencia)


pryr::where('analisis_correspondencia')

source("https://raw.githubusercontent.com/MangoTheCat/remotes/master/install-github.R")$value("mangothecat/functionMap")

remotes::install_github("MangoTheCat/functionMap")


mapa_funciones<-functionMap::map_r_package('../encuestar/',include_base = F)

lista_paqueterias<-mapa_funciones$node_df

paqueteria_listas <- lista_paqueterias|>
  as_tibble()|>
  filter(!is.na(file))|>
  mutate(lugar = gsub('R\\\\','',file))|>
  mutate(lugar = gsub('.R','',lugar))





lista_paqueterias_fin<-mapa_funciones$edge_df|>
  as_tibble()|>
  filter()









stringr::str_replace()



