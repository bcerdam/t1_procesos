# Parte 1.

library(quantmod)
# Descargar datos de Apple (AAPL) desde Yahoo Finance
getSymbols("AAPL", src = "yahoo", from = "2021-08-01", to = "2024-09-30")
aapl_data <- AAPL
head(aapl_data)

# Parte 2. dice "precio de cierre ajustado". Esta variable no existe, solamente existe de cierre o ajustado.
# Voy a ocupar el valor ajustado.

aapl_adjusted = aapl_data$AAPL.Adjusted
ganancias = dailyReturn(aapl_adjusted)
aapl_data$ganancias = ganancias

# Parte 3. 

entrenamiento = aapl_data["2021-08-02/2024-08-31"]
prueba = aapl_data["2024-09-01/2024-09-27"]


# Parte (a)

clasificar = function(dataset){
  ganancias_clasificadas = c()
  for(i in 1:nrow(dataset)){
    if(as.numeric(dataset$ganancias[i]) > -Inf & as.numeric(dataset$ganancias[i]) <= -0.01){
      ganancias_clasificadas[i] = 1
    }
    else if(as.numeric(dataset$ganancias[i]) > -0.01 & as.numeric(dataset$ganancias[i]) <= 0.01){
      ganancias_clasificadas[i] = 2
    }
    else if(as.numeric(dataset$ganancias[i]) > 0.01 & as.numeric(dataset$ganancias[i]) <= Inf){
      ganancias_clasificadas[i] = 3
    }
  }
  return(ganancias_clasificadas)
}

ganancias_clasificadas_entrenamiento = clasificar(entrenamiento)
ganancias_clasificadas_prueba = clasificar(prueba)

# Parte (b)

matriz_transicion = function(dataset){
  frecuencias_totales = c()
  for(i in 1:3){
    frecuencias = c()
    conteo_fila = 0
    for(j in 1:3){
      conteo_punto = 0
      for(k in 1:length(dataset)){
        if(k+1 == length(dataset)){
          break
        }
        par_n0 = dataset[k]
        par_n1 = dataset[k+1]
        if(i == par_n0 & j == par_n1){
          conteo_punto = conteo_punto + 1
        }
      }
      conteo_fila = conteo_fila + conteo_punto
      frecuencias = append(frecuencias, conteo_punto)
    }
    frecuencias_totales = append(frecuencias_totales, frecuencias*1/conteo_fila)
  }
  return (frecuencias_totales)
}

matriz_transicion_entrenamiento = matriz_transicion(ganancias_clasificadas_entrenamiento)
matriz_transicion_prueba = matriz_transicion(ganancias_clasificadas_prueba)

