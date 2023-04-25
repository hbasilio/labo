#Script Frankenstein

set.seed( 400199 )

#calcula cuantos encestes logra un jugador con indice de enceste prob
#haciendo qty tiros libres

ftirar  <- function( prob, qty ){
  return( sum( runif(qty) < prob ) )
}


experimentos_qty  <- 10000  #cantidad de veces que repito el experimento

ronda1_qty  <-   50      # cantidad de tiros de la primer ronda
ronda1_elegidos  <- 5    # cantidad que pasan a la segunda ronda
ronda2_qty  <-  100      # cantidad de tiros de la segunda ronda

#defino los jugadores
mejor      <- 0.8
peloton    <- ( 700:799 ) / 1000
jugadores  <- c( mejor, peloton )


#control
if( sum( mejor < peloton ) > 0 )  stop( "Atencion, hay jugadores en el peloton mejores que 'mejor'\nSTOP\n" )


verdadero_mejor_ganador  <- 0

for( veces in  1:experimentos_qty )
{
  jugadores  <- sample( jugadores )  #sampleo para evitar vicios

  #esta es la planilla del entrenador
  planilla  <- as.data.table( list( jugador = jugadores ) )
                                  

  # ronda1
  vaciertos1  <- mapply( ftirar, planilla$jugador, ronda1_qty )  
  planilla[  , ronda1_qty := ronda1_qty ]
  planilla[  , ronda1_aciertos := vaciertos1 ]
  planilla[  , ronda1_ratio := ronda1_aciertos/ronda1_qty]


  #marco a los que pasan
  setorder( planilla, -ronda1_aciertos )  # ordeno en forma descente por aciertos
  planilla[ , ronda2 := FALSE ] # inicializo que nadie ingresa a la ronda 2
  planilla[ 1:ronda1_elegidos, ronda2 := TRUE ]  # marco lo que ingresan a la ronda2

  # ronda2
  vaciertos2  <- mapply( ftirar, planilla[ ronda2==TRUE, jugador], ronda2_qty )  
  planilla[ ronda2==TRUE, ronda2_qty := ronda2_qty ]
  planilla[ ronda2==TRUE, ronda2_aciertos := vaciertos2 ]
  planilla[ ronda2==TRUE, ronda2_ratio := ronda2_aciertos/ronda2_qty]


  pos  <- planilla[ , which.max( ronda2_aciertos ) ]

  verdadero_mejor_ganador  <- verdadero_mejor_ganador  +  planilla[ pos, as.integer(jugador == mejor) ]
}


cat( "Se eligio al verdadero mejor :",  verdadero_mejor_ganador/experimentos_qty, "\n" )

