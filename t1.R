# Parte 1, lectura de datos. (Codigo Profe)

library(quantmod)
# Descargar datos de Apple (AAPL) desde Yahoo Finance
getSymbols("AAPL", src = "yahoo", from = "2021-08-01", to = "2024-09-30", auto.assign=FALSE)
aapl_data <- AAPL
head(aapl_data)

# Parte 2, Calculo de ganancias. Enunciado dice utilizar "precio de cierre ajustado", pero
# esta variable no existe, solamente existe de cierre o ajustado, pero no ambas al mismo tiempo. 
# Voy a ocupar el valor ajustado.

aapl_adjusted = aapl_data$AAPL.Adjusted
ganancias = dailyReturn(aapl_adjusted)
aapl_data$ganancias = ganancias

# Parte 3. Division entrenamiento y prueba.

entrenamiento = aapl_data["2021-08-02/2024-08-31"]
prueba = aapl_data["2024-09-01/2024-09-27"]


# Parte (a), clasificar ganancias en estados.

# clasificar(): Recibe un xts, itera las ganancias y las va clasificando, devuelve un vector con ganancias clasificadas.
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

# Parte (b), obtener matriz transicion de datos entrenamiento.

# matriz_transicion(): Recibe vector, no dataset. Itera todas las combinaciones posibles de estados ((1, 2, 3)x(1, 2, 3))
# y va contando cuantas veces se repite en los datos, luego, divide por fila para que sea una probabilidad y retorna 
# los valores de la matriz de transicion.
matriz_transicion = function(dataset){
  frecuencias_totales = c()
  for(i in 1:3){
    frecuencias = c()
    conteo_fila = 0
    for(j in 1:3){
      conteo_punto = 0
      for(k in 1:length(dataset)){
        par_n0 = dataset[k]
        par_n1 = dataset[k+1]
        if(i == par_n0 & j == par_n1){
          conteo_punto = conteo_punto + 1
        }
        if(k+1 == length(dataset)){
          break
        }
      }
      conteo_fila = conteo_fila + conteo_punto
      frecuencias = append(frecuencias, conteo_punto)
    }
    frecuencias_totales = append(frecuencias_totales, frecuencias*1/conteo_fila)
  }
  return (frecuencias_totales)
}


# formato_matriz(): Recibe las frecuencias de los estados que vienen de matriz_transicion(), y un parametro dim para
# especificar la dimension de la matriz de transicion, devuelve la matriz de transicion en un formato leible.
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
  colnames(P) <- c(1:dim) # Codigo del profe
  rownames(P) <- c(1:dim) # Codigo del profe
  return(P)
}

set.seed(2017)

matriz_transicion_entrenamiento = matriz_transicion(ganancias_clasificadas_entrenamiento)
matriz_transicion_prueba = matriz_transicion(ganancias_clasificadas_prueba)
P_entrenamiento = formato_matriz(matriz_transicion_entrenamiento, 3)
P_prueba = formato_matriz(matriz_transicion_prueba, 3)

# Descomentar para observar resultado de matrices
# P_entrenamiento
# P_prueba

# Parte (c), Generamos vectores de estados para dias de septiembre. (En su mayoria, codigo del profresor)

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

# Descomentar para observar vectores de estados para dias de septiembre.
# Pn

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

# Descomentar para observar histograma de promedio de resultados.
# hist(vector_promedios)

# parte (d), verificar si tiene comportamiento leptocurtico, auto correlacion.

# comportamiento leptocurtico => verificar si kurtosis es positiva
# Tambien visualmente podemos ver si tiene colas pesadas y una concentracion central mas grande.

# install.packages('moments')
library(moments)
ganancias_kurtosis = kurtosis(aapl_data$ganancias) # Positiva y no se aproxima a 0 -> comportamiento leptocurtico.

# Descomentar para observar.Tiene alta concentracion central, "pico alto", colas pesadas si.
# hist(aapl_data$ganancias) 

# parte (e), ajustar distribucion, por EMV, que tenga caracterisitcas leptocurticas, y simular matriz transicion a partir de esta.

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
colnames(valores_simulados) = 'ganancias'

P_ajustada = formato_matriz(matriz_transicion(clasificar(valores_simulados)), 3)

# Descomentar para Comparacion matriz transicion ajustada y entrenamiento.
P_entrenamiento
P_ajustada

# Yo diria que la diferencia no es tanta. Lo que hice fue restar ambas matrices y calcular desviacion estandar de las diferencias.
diferencias = c(0.10015765, -0.04666316, -0.053494482, -0.04428668, 0.04828114, -0.003994462, -0.01185293, 0.02123851, -0.009385583)
desviacion_estandar_diferencias = sd(diferencias)


