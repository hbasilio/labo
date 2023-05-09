require("data.table")

#limpio la memoria
rm( list= ls(all.names= TRUE) )  #remove all objects
gc( full= TRUE )                 #garbage collection
#
# Cargo el Data Set de la competencia
dataset_grande <- fread("~/buckets/b1/datasets/competencia_2023.csv.gz") 
#
# Correr desde aca el script
#
#
# Cambiar para cada salida de script ZZ final y cada semilla , carpeta y predicción de modelo 1
dataset_pred <- fread("~/buckets/b1/exp/ZZ691-3B5/pred_01_075.csv")

dataset_pred[dataset_grande,
             on= c("numero_de_cliente","foto_mes"),
             clase_ternaria:=i.clase_ternaria]

dataset_pred[,ganancia:=-3000]
dataset_pred[clase_ternaria=="BAJA+2",ganancia:=117000]
setorder(dataset_pred,-prob)

dataset_pred[,ganancia_acumulada:=cumsum(ganancia)]

dataset_pred[,envios:=.I]

# Cambiar la carpeta de salida i el archivo
fwrite(dataset_pred[1:20000],"~/buckets/b1/exp/AA701/3B5_pred_01_075_graficar.csv")