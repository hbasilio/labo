# Arbol elemental con libreria rpart
# Agregar Random Forest
# Debe tener instaladas las librerias data.table, rpart, rpart.plot, randomForest

# cargo las librerias que necesito
require("data.table")
require("rpart")
require("rpart.plot")
require("randomForest")

# Aqui se debe poner la carpeta de la materia de SU computadora local
setwd("C:\\Lab_Imp_1\\") # Establezco el Working Directory

# cargo el dataset
dataset  <- fread(".\\datasets\\dataset_pequeno.csv")

dtrain  <- dataset[foto_mes == 202107] # defino donde voy a entrenar
dapply  <- dataset[foto_mes == 202109] # defino donde voy a aplicar el modelo

# Imputacion de los valores nulos
# Calculando la media
#dtrain[is.na(dtrain)] <- mean(dtrain, na.rm = TRUE) # Imputar con la media de cada variable
# o boorando las filas nulas
dtrain <- na.omit(dtrain) # Eliminar filas con valores faltantes


# genero el modelo de Random Forest
modelo_rf <- randomForest(clase_ternaria ~ ., # quiero predecir clase_ternaria a partir de el resto de las variables
                          data = dtrain, # los datos donde voy a entrenar
                          ntree = 500, # número de árboles en el bosque
                          mtry = sqrt(ncol(dtrain)-1), # número de variables aleatorias evaluadas en cada nodo
                          importance = TRUE, # calcular la importancia de las variables
                          proximity = TRUE) # calcular la matriz de proximidad entre observaciones

# grafico la importancia de las variables
varImpPlot(modelo_rf)

# aplico el modelo a los datos nuevos
prediccion_rf <- predict(object = modelo_rf,
                         newdata = dapply,
                         type = "prob")

# prediccion_rf es una matriz con TRES columnas, llamadas "BAJA+1", "BAJA+2" y "CONTINUA"
# cada columna es el vector de probabilidades 

# agrego a dapply una columna nueva que es la probabilidad de BAJA+2
dapply[, prob_baja2 := prediccion_rf[, "BAJA+2"]]

# solo le envio estimulo a los registros con probabilidad de BAJA+2 mayor a 1/40
dapply[, Predicted := as.numeric(prob_baja2 > 1/40)]

# genero el archivo para Kaggle
# primero creo la carpeta donde va el experimento
# dir.create("./exp/")
# dir.create("./exp/KA2001")

fwrite(dapply[, list(numero_de_cliente, Predicted)], # solo los campos para Kaggle
       file = "./exp/KA2001/K101_RF_013.csv",
       sep = ",")

