library(dplyr)

ruta_archivo <- "./R/clases_surveytogo.R"
script <- readLines(ruta_archivo)

# Buscar todas las clases definidas en el script
patron_clase <- "<- R6::R6Class"
lineas_clases <- grep(patron_clase, script)

# Buscar todas las funciones definidas en el script
patron_funciones <- "= function"
lineas_funciones <- grep(patron_funciones, script)


# Identificar las líneas de inicio y fin de cada función
resultado_clases <- data.frame(
  clase = character(),
  linea_clase_inicio = integer(),
  linea_clase_fin = integer(),
  funcion = character(),
  linea_metodo_inicio = integer(),
  linea_metodo_fin = integer(),
  stringsAsFactors = FALSE
)

# Función para identificar el cierre de una clase
buscar_fin_clase <- function(script, ln_inicio, llave_apertura, llave_clausura) {
  # Contador de llaves abiertas y cerradas
  contador_llaves <- 0
  for(i in ln_inicio:length(script)) {
    # Contar llaves abiertas y cerradas
    contador_llaves <- contador_llaves + sum(stringr::str_count(script[i], llave_apertura)) - sum(stringr::str_count(script[i], llave_clausura))
    # Si el contador vuelve a 0, significa que la función ha terminado
    if(contador_llaves == 0) {
      return(i)
    }
  }
  return(NA)  # En caso de que no se encuentre el cierre (debería evitarse si el script es válido)
}


# Procesar cada línea donde comienza una función
for(ln_inicio_clase in lineas_clases) {
  # Obtener el nombre de la clase (antes de '<- R6::R6Class')
  linea_texto <- script[ln_inicio_clase]
  nombre_clase <- strsplit(linea_texto, "<-")[[1]][1]
  nombre_clase <- trimws(nombre_clase)

  # Buscar la línea de finalización de la función
  ln_fin_clase <- buscar_fin_clase(script, ln_inicio_clase,"\\(","\\)")

  for(ln_inicio_funcion in  lineas_funciones[lineas_funciones<=ln_fin_clase & lineas_funciones >=ln_inicio_clase] ){
    # Obtener el nombre de la funcion (antes de '= function')
    linea_texto <- script[ln_inicio_funcion]
    nombre_funcion <- strsplit(linea_texto, "<-")[[1]][1]
    nombre_funcion <- trimws(nombre_funcion)

    # Buscar la línea de finalización de la función
    ln_fin_funcion <- buscar_fin_clase(script, ln_inicio_funcion,"\\{","\\}")

    # Agregar al resultado
    resultado_clases <- rbind(resultado_clases, data.frame(
      clase = nombre_clase,
      linea_clase_inicio = ln_inicio_clase,
      linea_clase_fin = ln_fin_clase,
      funcion = nombre_funcion,
      linea_metodo_inicio = ln_inicio_funcion,
      linea_metodo_fin = ln_fin_funcion
    ))

  }


}




