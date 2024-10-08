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

formato_matriz = function(matriz, dim){
  P = matrix(nrow=dim, ncol=dim)
  contador = 0
  temp = c()
  for(i in 1:dim^2){
    temp = append(temp, matriz[i])
    contador = contador + 1
    if (contador == 3){
      P[i/3, ] = temp
      temp = c()
      contador = 0
    }
  }
  colnames(P) <- c(1:dim)
  rownames(P) <- c(1:dim)
  return(P)
}

#set.seed(123456)
set.seed(2017)

matriz_transicion_entrenamiento = matriz_transicion(ganancias_clasificadas_entrenamiento)
matriz_transicion_prueba = matriz_transicion(ganancias_clasificadas_prueba)
P_entrenamiento = formato_matriz(matriz_transicion_entrenamiento, 3)
P_prueba = formato_matriz(matriz_transicion_prueba, 3)

# Parte (c)

pi.0 = c(0, 1, 0)
D <- diag(eigen(P_entrenamiento)$value)
A <- eigen(P_entrenamiento)$vect

Pn <- c()
for(k in 1:nrow(prueba)){
  pi.k <- pi.0%*%A%*%D^k%*%solve(A)
  Pn <- rbind(Pn, pi.k)
}
rownames(Pn) <- 1:nrow(prueba)
colnames(Pn) <- 1:3
Pn

# simulacion
vector_promedios = c()
for(i in 1:1000){
  y <- c()
  for(n in 1:nrow(prueba)){
    y[n] <- sample(1:3, prob= Pn[n,], size = 1)
  }
  promedio = sum(ganancias_clasificadas_prueba == y) / length(ganancias_clasificadas_prueba)
  vector_promedios = append(vector_promedios, promedio)
}

hist(vector_promedios)

# parte (d)

# comportamiento leptocurtico => verificar si kurtosis es positiva
# Tambien visualmente podemos ver si tiene colas pesadas y una concentracion central mas grande.

# install.packages('moments')
library(moments)
ganancias_kurtosis = kurtosis(aapl_data$ganancias) # Positiva y no se aproxima a 0 -> comportamiento leptocurtico.

hist(aapl_data$ganancias) # Concentracion central si, "pico alto" si, colas pesadas si.

# Yo diria que si tiene comportamiento leptocurtico.

# No se a que se refiere con: 'baja o nula auto-correlación serial y auto-correlación serial significativa en sus cuadrados.'


# parte (e)

# install.packages("fitdistrplus")
library(fitdistrplus)

# Ajuste
fit = fitdistr(as.numeric(aapl_data$ganancias), "t")

# Parametros Ajuste
m = as.numeric(fit$estimate)[1]
s = as.numeric(fit$estimate)[2]
df = as.numeric(fit$estimate)[3]

# Plotteo del resultado del ajuste
hist(aapl_data$ganancias, probability=TRUE)
curve(dt((x - m)/s, df)/s, add=TRUE, from=min(aapl_data$ganancias), to=max(aapl_data$ganancias))

# Simulacion
samples = m + s * rt(nrow(aapl_data$ganancias), df)

library(xts)
fechas = seq(as.Date("2022-08-01"), by = "day", length.out = nrow(aapl_data$ganancias))
valores_simulados = xts(samples, fechas)
colnames(valores_simulados) <- 'ganancias'

P_ajustada = formato_matriz(matriz_transicion(clasificar(valores_simulados)), 3)

# Comparacion matriz transicion ajustada y entrenamiento.
P_entrenamiento
P_ajustada


