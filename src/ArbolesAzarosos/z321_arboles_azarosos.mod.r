#Ensemble de arboles de decision
#utilizando el naif metodo de Arboles Azarosos
#entreno cada arbol utilizando un subconjunto distinto de atributos del dataset

#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

require("data.table")
require("rpart")

#parmatros experimento
PARAM <- list()
PARAM$experimento  <- 230418
PARAM$semilla  <- 500129     #Establezco la semilla aleatoria, cambiar por SU primer semilla

#parameetros rpart
PARAM$rpart_param   <- list( "cp"=          -1,
                              "minsplit"=  1525,
                              "minbucket"=  401,
                              "maxdepth"=     9 )

#parametros  arbol
PARAM$feature_fraction  <- 0.33  #entreno cada arbol con solo 50% de las variables variables
PARAM$num_trees_max  <- 500 #voy a generar 500 arboles, a mas arboles mas tiempo de proceso y MEJOR MODELO, pero ganancias marginales


#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Aqui comienza el programa

setwd("C:\\Lab_Imp_1\\")  #Establezco el Working Directory

#cargo los datos
dataset  <- fread("./datasets/dataset_pequeno.csv")


#creo la carpeta donde va el experimento
dir.create( "./exp/", showWarnings = FALSE  )
carpeta_experimento  <-  paste0( "./exp/HB", PARAM$experimento, "/")
dir.create( paste0( "./exp/HB", PARAM$experimento, "/"), 
            showWarnings = FALSE )

setwd( carpeta_experimento )


#que tamanos de ensemble grabo a disco, pero siempre debo generar los 500
grabar  <-  c( 1, 5, 10, 50, 100, 200, 500)


#defino los dataset de entrenamiento y aplicacion
dtrain  <- dataset[ foto_mes==202107 ]
dapply  <- dataset[ foto_mes==202109 ]

#aqui se va acumulando la probabilidad del ensemble
dapply[ , prob_acumulada := 0 ]

#Establezco cuales son los campos que puedo usar para la prediccion
#el copy() es por la Lazy Evaluation
campos_buenos  <- copy( setdiff(  colnames(dtrain) ,  c("clase_ternaria") ) )



#Genero las salidas
set.seed(PARAM$semilla) #Establezco la semilla aleatoria

for( arbolito in  1:PARAM$num_trees_max )
{
  qty_campos_a_utilizar  <- as.integer( length(campos_buenos)* PARAM$feature_fraction )
  campos_random  <- sample( campos_buenos, qty_campos_a_utilizar )
  
  #paso de un vector a un string con los elementos separados por un signo de "+"
  #este hace falta para la formula
  campos_random  <- paste( campos_random, collapse=" + ")

  #armo la formula para rpart
  formulita  <- paste0( "clase_ternaria ~ ", campos_random )

  #genero el arbol de decision
  modelo  <- rpart( formulita,
                    data= dtrain,
                    xval= 0,
                    control= PARAM$rpart_param )

  #aplico el modelo a los datos que no tienen clase
  prediccion  <- predict( modelo, dapply , type = "prob")
  
  dapply[  ,  prob_acumulada :=  prob_acumulada + prediccion[ , "BAJA+2"] ]

  if( arbolito %in%  grabar )
  {
    #Genero la entrega para Kaggle
	umbral_corte  <-  (1/40) *  arbolito
    entrega  <- as.data.table( list( "numero_de_cliente"= dapply[  , numero_de_cliente],
                                     "Predicted"= as.numeric(dapply[  , prob_acumulada] > umbral_corte) ) ) #genero la salida

    nom_arch  <- paste0('FB', PARAM$experimento, "_",
						sprintf( '%.3d', arbolito ), #para que tenga ceros adelante
						'.csv' )
    fwrite( entrega, 
            file= nom_arch,
            sep= "," )

    cat( arbolito, " " )
  }
}
wilcox.test(c(49.11911, 46.70915, 46.79915, 48.64912, 47.02915, 47.07914, 47.65913, 47.54914, 48.00913,46.46916,
              47.75913, 46.52915, 46.89915, 45.96916, 47.31914, 47.9392,  42.54923, 48.44, 49.08911, 46.70915,
              48.18912, 46.79915, 46.78915, 44.86918, 47.65913, 47.44914, 48.06913, 46.14916, 43.70913, 46.01916,
              45.43917, 48.23912, 46.46916, 46.71915, 45.89917, 47.13914, 46.25919, 45.02918), 
            c(50.10909, 47.23914, 46.71915, 47.61913, 47.21914, 46.39916, 47.57914, 47.54914, 46.84915, 45.53917,
              47.43914, 46.26916, 45.66917, 46.37916, 46.62915, 45.64595, 47.85913, 47.56, 47.78913, 47.23914, 47.65913,
              46.71915, 47.97913, 45.51917, 47.81913, 46.85915, 49.10911, 47.64913, 46.88915,46.78915,47.58914, 49.5891,
              47.08914, 46.79915, 47.99913, 47.61913, 46.51151, 45.76917))


