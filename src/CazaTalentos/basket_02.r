#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()             #garbage collection
#

set.seed( 400199 )

#calcula cuantos encestes logra un jugador con indice de enceste prob
#haciendo qyt tiros libres

ftirar  <- function( prob, qty ){
  return( sum( runif(qty) < prob ) )
}


#defino los jugadores
mejor      <- 0.85
#peloton    <- ( 724:829 ) / 1000
#print (peloton)
# Caso 2
#jugadores  <- c( mejor, 0.79, 0.79, 0.79, 0.79, 0.79, 0.79, 0.78, 0.78, 0.78, 0.78, 0.78, peloton )
# Caso 3
#jugadores  <- c( 0.8, 0.75 )
# Caso 4
#jugadores  <- c( mejor, peloton )
# Caso 6
#jugadores  <- c( 0.8, 0.79, 0.78, 0.77, 0.72 )
# Caso 7
# El caso es con combinatoria de 100 tomados de 5
#jugadores  <- c( 0.8, 0.78, 0.78, 0.76, 0.74 )
jugadores  <- c( 0.85, 0.83, 0.83, 0.81, 0.79 )
# Caso 8
#jugadores  <- c( 0.79985, 0.84, 0.84, 0.83, 0.83, 0.83, peloton )
# Caso 9
#jugadores  <- c( 0.81, 0.82 )

#veo que tiene el vector
jugadores


#hago que los "n" jugadores tiren 100 veces cada uno
mapply( ftirar, jugadores, 105 )

primero_ganador  <- 0

for( i in 1:10000 ){  #diez mil experimentos

  vaciertos  <- mapply( ftirar, jugadores, 105 )  #100 tiros libres cada jugador

  mejor  <- which.max( vaciertos )
  if( mejor == 1 )  primero_ganador  <- primero_ganador + 1
}


print(  primero_ganador )
#
fisher.test(matrix(c(20,80 , 75, 25), ncol=2))

