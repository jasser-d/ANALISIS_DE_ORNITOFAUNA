#ESTADITICA DE INVENTARIO

#librerias utiliuzadas dentro del script--------------------------------------------------------------------

#library(asbio)
#library(dplyr)
#library(plotrix)

#funcion que se usa para el calculo estadistico de la estadsitica descriptiva------------------------------

ESTADISTICA_DESCRIPTIVA<-function(DATOS){
  
  #estadsitica descriptiva
  
  DATOS<-c(DATOS)
    #numero total de parcelas
    n_total<-as.numeric(length(DATOS))
    
    #numero total de observaciones
    total<-sum(DATOS)
    
    #promedio de observaciones por parcela
    prom<-mean(DATOS)
    
    #valor maximo observado por parcela
    maxim<-max(DATOS)
    
    #valor minimo observado por parcela
    minim<-min(DATOS)
    
    #varianza de las parcelas 
    varian<-var(DATOS)
    
    #desviasion estandar de las parcelas
    desv_est<-sd(DATOS)
    
    #error estandar de una muestra
    error_str<-std.error(DATOS)
    
    #rango de observaciones de una parcela
    rango<-maxim-minim
    
    #mediana
    mediana<-median(DATOS)
  
    #intervalo de confianza 
    inter_conf<-1.96*(desv_est/sqrt(n_total))
    
    #limite maximo
    inter_max<-prom+inter_conf
    
    #limite minimo de una poarcela
    inter_min<-prom-inter_conf
    
    #coeficiente de variavion de de las parcelas
    CV<-(desv_est/prom)*100
    
    #error muestral de lauestra
    error_m<-(inter_conf/prom)*100
  
  #unificacion de variables estadistica en una tabla de estadsitica descriptiva
  BASE_EST<-rbind('TOTAL DE PUNTOS DE OBSERVACION'=n_total,'TOTAL DE INDIVIDUOS OBSERVADOS'=total,PROMEDIO=prom,MAXIMO=maxim,MINIMO=minim,
                  VARIANZA=varian,'DESVIACION ESTANDAR'=desv_est,'ERROR DE LA MEDIA'=error_str,RANGO=rango,MEDIANA=mediana,'INTERVALO DE CONVIANZA'=inter_conf,
                  'LIMITE MAXIMO'=inter_max,'IMITE MINIMO'=inter_min,'COEFICIENTE DE VARIACION %'=CV,'ERROR MUESTRAL'=error_m)
  
  colnames(BASE_EST)<-c("ESTADISTICOS")
  
  return(BASE_EST)
}




