require("data.table")


dataset_grande <- fread("~/buckets/b1/datasets/competencia_2023.csv.gz") 
#
# Cambiar para cada salida de script ZZ final y cada semilla
dataset_pred <- fread("~/buckets/b1/exp/ZZ691-3A1/pred_01_040.csv")

dataset_pred[dataset_grande,
             on= c("numero_de_cliente","foto_mes"),
             clase_ternaria:=i.clase_ternaria]

dataset_pred[,ganancia:=-3000]
dataset_pred[clase_ternaria=="BAJA+2",ganancia:=117000]
setorder(dataset_pred,-prob)

dataset_pred[,ganancia_acumulada:=cumsum(ganancia)]

dataset_pred[,envios:=.I]

fwrite(dataset_pred[1:20000],"~/buckets/b1/exp/ZZ691-3A1/pred_01_040_graficar.csv")