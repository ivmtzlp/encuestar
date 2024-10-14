analizar_frecuenciasRegion <- function(regiones, variable, diseno){
  variable = variable
  formula <- survey::make.formula(rlang::expr_text(ensym(variable)))
  tbl <- regiones %>%
    left_join(
      survey::svyby(formula, ~region, design = diseno, FUN = survey::svymean, na.rm  = T) %>%
        as_tibble(), by = "region")
  return(tbl)
}


encuesta_demo$Resultados$Regiones$shp_regiones



frecs_region<-analizar_frecuenciasRegion(diseno = encuesta_demo$muestra$diseno,
                                         regiones =  encuesta_demo$Resultados$Regiones$shp_regiones,
                                         variable = 'voto_pm_24' )





variable <- 'voto_pm_24'
lugar <- 1



frecs_region|>
  janitor::clean_names() |>
  as_tibble() |>
  select(!c(geometry, n)) |>
  select(!contains("se")) |>
  tidyr::pivot_longer(cols = !region, names_to = rlang::expr_text(ensym(variable)), values_to = "estimacion") |>
  group_by(region) %>%
  filter(dense_rank(-estimacion) == lugar)
tbl <- regiones %>%
  left_join(bd_topEstimaciones, by = "region")






calcular_ganadorRegion <- function(diseno, regiones, variable, lugar){
  bd_topEstimaciones <- analizar_frecuenciasRegion(regiones = regiones, variable = variable, diseno = diseno) |>
    janitor::clean_names() |>
    as_tibble() |>
    select(!c(geometry, n)) |>
    select(!contains("se")) |>
    tidyr::pivot_longer(cols = !region, names_to = rlang::expr_text(ensym(variable)), values_to = "estimacion") |>
    group_by(region) %>%
    filter(dense_rank(-estimacion) == lugar)
  tbl <- regiones %>%
    left_join(bd_topEstimaciones, by = "region")
  return(tbl)
}

ganador_region <-  calcular_ganadorRegion(diseno = encuesta_demo$muestra$diseno,
                                          regiones = encuesta_demo$Resultados$Regiones$shp_regiones,
                                          variable = 'voto_pm_24',lugar = 1)


ganador_region_2 <-
  encuesta_demo$Resultados$Regiones$shp_regiones|>
  left_join(
  encuestar:::analizar_cruce(diseno = encuesta_demo$muestra$diseno,
                             variable_principal = 'region',
                             variable_secundaria = 'voto_pm_24',
                             vartype = 'cv',na_rm = T)|>
    group_by(region)|>
    filter(dense_rank(-coef) == lugar)|>
    select(- c('_cv','pres')),
  by = 'region')|>
  filter(!is.na(region))



ganador_region|>
  graficar_mapaRegiones(variable = 'voto_pm_24',categorica = F)



ganador_region_2|>
  graficar_mapaRegiones(variable = 'voto_pm_24',categorica = T)





mapa_ganador = function(variable,region = 'region', lugar = 1, na_rm = T){

  if(is.null(self$diseno)) {

    diseno <- self$encuesta$muestra$diseno

  } else {

    diseno <- self$diseno

  }


  self$shp|>
    left_join(
      encuestar:::analizar_cruce(diseno = diseno,
                                 variable_principal = region,
                                 variable_secundaria = variable,
                                 vartype = 'cv',na_rm = na_rm)|>
        group_by(region)|>
        filter(dense_rank(-coef) == lugar)|>
        select(- c('_cv','pres')),
      by = 'region')|>
    filter(!is.na(region))%>%
    graficar_mapaRegiones(variable = {{variable}})
}


encuesta_demo$Resultados$Regiones$mapa_ganador(variable = 'voto_pm_24',lugar = 1)

encuesta_demo$muestra$diseno$variables













