# Parte 1.

library(quantmod)
# Descargar datos de Apple (AAPL) desde Yahoo Finance
getSymbols("AAPL", src = "yahoo", from = "2021-08-01", to = "2024-09-30")
aapl_data <- AAPL
head(aapl_data)

# Parte 2. dice "precio de cierre ajustado". Esta variable no existe, solamente existe de cierre o ajustado.
# Voy a ocupar el valor ajustado.
aapl_adjusted = aapl_data$AAPL.Adjusted
ganancias = c()

for(i in 1:nrow(aapl_adjusted)){
  ganancias[i] = as.numeric(aapl_adjusted[i+1]) - as.numeric(aapl_adjusted[i])
  if(i+1 == length(aapl_adjusted)){
    break
  }
}

# Parte 3. 

entrenamiento = aapl_data["2021-08-02/2024-08-31"]
prueba = aapl_data["2024-09-01/2024-09-27"]


# Parte (a)

ganancias_clasificadas = c()
for(i in 1:length(ganancias)){
  if(ganancias[i] > -Inf & ganancias[i] <= -0.01){
    ganancias_clasificadas[i] = 1
  }
  else if(ganancias[i] > -0.01 & ganancias[i] <= 0.01){
    ganancias_clasificadas[i] = 2
  }
  else if(ganancias[i] > 0.01 & ganancias[i] <= Inf){
    ganancias_clasificadas[i] = 3
  }
}

# Hay muy pocos que estan en estado '2', medio sospechoso.
