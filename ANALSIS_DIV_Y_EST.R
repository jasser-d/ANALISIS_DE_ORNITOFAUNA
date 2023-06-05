#INSTALE LAS SIGUIENTES LIBRERIAS Y SUS DEPENDENCIAS----------------------------------------------------------------------
"
install.packages('tcltk')
install.packages('permute')
install.packages('lattice')
install.packages('vegan')
install.packages('DT')
install.packages('asbio')
install.packages('plotrix')
install.packages('dplyr')
install.packages('ggplot2')
install.packages('patchwork')
install.packages('ggwordcloud')
"
  #librerias___________________________________________________________________________________________

library(renv)
library(tcltk)
library(permute)
library(lattice)
library(vegan)
library("DT")
library(asbio)
library(plotrix)
library(dplyr)
library(ggplot2)
library(patchwork)
library(ggwordcloud)


#cargar script---------------------------------------------------------------------------------------------

#estos scripts permiten ejecutar las siguientes funciones que son necesarias para el funcionamiento coorrecto o ejecucion de algun paso necesiario para la
#el analisis de la avifauna, estos tambien puedne funcionar para inventarios de fauna y flora realizando modificaciones en la entrada de datos en la
#esctructura de los algoritmos 


  #estadistica descriptiva
    source("SCRIPS/ESTADISTICA_DESCRIPTIVA.R")

  #adactar datos para definir el marco de datos que calcula la suma de una variable en funcion de dos grupos
    source("SCRIPS/TRANSFORMACION.R")

  #script de diversidad de para cada parcela de muestreo
    source("SCRIPS/DIVERSIDAD_ALFA.R")

  #script que calcula la abudancia en funcion de las observaciones de las especies y su densidad
    source("SCRIPS/ABUNDANCIA_DENSIDAD_ESPECIE.R")

  #script que permite crear una grafica del numero de observaciones por especie y mes
    source("SCRIPS/FECHA_OBSERVACIONES.R")

  #script que calcula los dtistintos modelos de curva de acumulacion en una tabla con lña libreria Vegan
    source("SCRIPS/CURVA_ACUMULACION.R")

  #script que define los maximos y minimos que presenta las filas de una tabla
    source("SCRIPS/MAXIMO_MINIMO_VALOR.R")

  #script que define la abundacia y densidad segun los puntos de observacion fijados
    source("SCRIPS/ABUNDANCIA_DENSIDAD_PUNTO_OBSERVACION.R")
  
#cargar base de datos y ruta de archvos de resultados--------------------------------------------------------

#el archivo de be de estar en formato .csv delimitado por comas (esto se puede modificar), pero las columnas deben de presentar los sguientes nombres
"
ID = identificador unico para cada especie observada en la parcela en un moneto dado (dia)

OBSERVACIONES = un codigo unico que identifique la observacion realizada en un periodo dado dentro de una parcela (en este caso se realizo conla semana
que se realizo la observacion y el identificador de punto de observacion), no obstante en el caso de un inventario forestal o de otro indole, donde el
tiempo no afecte las observaciones, es decir no varien en un periodo corto de tiempo

SEMANA = Esta variable es opcional ya que no se usa en el analisis

FECHA = este es opcional por ahora ya que se planea en un futuro definir el numero de ocurrencia de cada especie en funcion del mes que se observo
el formato que debe de estar la fecha es dd/mm/aaaa

PUNTO DE OBSERVACION = Define un codigo unico que puede utilizarse para identificar los puntos de medicion realizados

NOMBRE COMUN = El nombre comun que tiene la especie, no se requiere para este script, pero se puede el cript para que conste esta informacion 

NOMBRE CIENTIFICO = El nombre cientifico que posee una especie determinada, es necesario para los distintos analisis que se realizaran

NUMERO DE AVISTAMIENTO = EL NUMERO TOTAL DE AVISTAMIENTO QUE SE HAN REALIZADO DE UNA ESPECIE, DENTRO DE UNA PARCELA Y EN UN PERIODO DETERMINADO

EN EL CASO DE NO REALIZAR UN INVENTARIO DE AVIFAUNA DE DOBLE BANDA NO SE DEBEN DE USAR LAS SIGUIENTES VARIABLES Y SE DEBE MODIFICAR VARIOS CRIPTS,YA
QUE ESTOS TRABAJAN EN BASE DE ESTAS OBSERVACIONES

DENTRO DEL RADIO = Se registran las especies observadas dentro de un radio definido como por ejemeplo  30m, teniendo lo establecido en las condiciones
de numero de avistamientos

FUERA DEL RADIO = Se registran las especies observadas fuera de un radio definido como por ejemeplo  30m, teniendo lo establecido en las condiciones
de numero de avistamientos
"

  #ruta de archivo para analsiis en formato .csv seprarados con punto y coma (;)
    DATOS_BASE<-read.csv(file = "DATOS_DE_EJEMPLO/DATOS_PRUEBA.csv",sep = ";",)
  
  #se debe definrir la superficie de muestreo de cada punto de observacion o parcela si desea modificar el scritp para o otros inventarios  
    superfice_muestreo<-pi*(30^2)
    
  #numero de observaciones realizadas
    CONTEOS_TOTAL<-as.numeric(length(unique(DATOS_BASE$OBSERVACIONES)))
    
  #rellenar datos vacios y dar formato de fecha
    
FECHAS<-as.Date(DATOS_BASE$FECHA,format = "%y/%m/%d")

DATOS_BASE<-DATOS_BASE%>%select(-FECHA)%>%data.frame(FECHA=FECHAS)

DATOS_BASE[is.na(DATOS_BASE)]<-0


#ESTADISTICA DEL VINVENTARIO DE LA ORNITOFAUNA------------------------------------------------------------------------

"
Se ejecuto un analisis de la estadistica descriptiva para los puntos de onbservacion con respecyo al numero total de encuentros
con aves que se registraron dando como resultado a los siguientes estaditicos:


"

TABLA_ESTADISTICA_DESCRIPTIVA<-DATOS_BASE%>%select(PUNTO.DE.OBSERVACION,NUMERO.DE.AVISTAMIENTO)%>%group_by(PUNTO.DE.OBSERVACION)%>%summarise('TOTAL DE AVISTAMIENTOS'=sum(NUMERO.DE.AVISTAMIENTO))

TABLA_ESTADISTICA_DESCRIPTIVA<-ESTADISTICA_DESCRIPTIVA(TABLA_ESTADISTICA_DESCRIPTIVA$`TOTAL DE AVISTAMIENTOS`)

print(TABLA_ESTADISTICA_DESCRIPTIVA)

#se puede modificar el script para eliminar otras estadisticas descriptivas y el enunciado para adactarlo al trabajo a realizar


#.....................................................................................................................


#DIVERSIDAD ALFA------------------------------------------------------------------------------------------------------

#preprosesamiento de los datos para dar un formato establecido para el analisis de diversidad con la libreria VEGAN,
#donde las columnas estan compuestas por las especies regisntradas y las filas por los puntos de observaciones y
#los valores que toman cada celda es el del numero de veces que se observo la especie


FORMATO_DIVERSIDAD<-transformacion_de_datos(filas = DATOS_BASE$PUNTO.DE.OBSERVACION,columnas =  DATOS_BASE$NOMBRE.CIENTIFICO,valor =  as.numeric(DATOS_BASE$NUMERO.DE.AVISTAMIENTO))

#DIVERCIDAD TOTAL DEL INVENTARIO Y POR PARCELA

DIVERSIDAD_TOTAL_ESTUDIO<-DIVERSIDAD_ALFA(FORMATO_DIVERSIDAD)

DIVERSIDAD_TOTAL<-DIVERSIDAD_TOTAL_ESTUDIO[[1]]
DIVERSIDAD_PARCELA<-DIVERSIDAD_TOTAL_ESTUDIO[[2]]

colnames(DIVERSIDAD_PARCELA)<-c("PUNTO DE OBSERVACION","DIVERSIDAD DE SHANNON","DIVERSIDAD DE SIMPSON")

print("
      DIVERSIDAD ALFA PARAR EL TOTAL DEL INVENTARIO SEGUN LOS INDICES DE SHANNON Y SIMPSON
      ")
print(DIVERSIDAD_TOTAL)

print("
      DIVERSIDAD ALFA PARA LOS PUNTOS DE OBSERVACION SEGUN LOS INDICES DE SHANNON Y SIMPSON
      ")
print(DIVERSIDAD_PARCELA)


#GRAFICA DE INDICES POR PARCELA DE SIMPSON Y SHANNOM
#.................................................................................................................

#establecer datos para trabajar con ggplot2

  #grafica de SHANNOM
  #...........................................................................................................

G1<-data.frame(x=DIVERSIDAD_PARCELA$`PUNTO DE OBSERVACION`,y=DIVERSIDAD_PARCELA$`DIVERSIDAD DE SHANNON`)

G1<-ggplot(G1)+
  geom_point(aes(x=x,y=y),color="blue")+
  ggtitle("DIVERSIDAD DE SHANNON POR PUNTO")+
  labs(x="PARCELAS",y="INDICE")+
  theme(
    title = element_text(size = 10,face="bold",color = "blue"),
    panel.background =element_rect(fill = "white"),
    panel.border = element_rect(fill="transparent",color="black",size = 1),
    panel.grid = element_line(color = "gray",size=0.5,linetype = 3),
    axis.title = element_text(size=7,face = "italic",color = "black"),
    axis.text = element_text(size=5,face = "bold")
  )

  #grafica de SIMPSON
  #..........................................................................................................

G2<-data.frame(x=DIVERSIDAD_PARCELA$`PUNTO DE OBSERVACION`,y=DIVERSIDAD_PARCELA$`DIVERSIDAD DE SIMPSON`)

G2<-ggplot(G2)+
  geom_point(aes(x=x,y=y),color="red")+
  ggtitle("DIVERSIDAD DE SHANNON POR PUNTO")+
  labs(x="PARCELAS",y="INDICE")+
  theme(
    title = element_text(size = 10,face="bold",color = "red"),
    panel.background =element_rect(fill = "white"),
    panel.border = element_rect(fill="transparent",color="black",size = 1),
    panel.grid = element_line(color = "gray",size=0.5,linetype = 3),
    axis.title = element_text(size=7,face = "italic",color = "black"),
    axis.text = element_text(size=5,face = "bold")
  )
#GRAFICA

G1|G2

#ABUNDANCIA y densidad-------------------------------------------------------------------------------------------------------------------------------

#......................................................................................................
#densidad  y abundancia por especie

  ABUND_DENC_ESPECIES<-ABUNDANCIA_DENCIDAD_ESPECIES(NOMBRE_CIENTIFICO = DATOS_BASE$NOMBRE.CIENTIFICO,TOTAL_AVISTAMIENTOS = DATOS_BASE$NUMERO.DE.AVISTAMIENTO,AVISTAMIENTOS_DENTRO_DEL_RADIO = DATOS_BASE$DENTRO.DEL.RADIO,AVISTAMIENTOS_FUERA_DEL_RADIO = DATOS_BASE$FUERA.DEL.RADIO,SUPERFICIE =  superfice_muestreo,CONTEOS =  CONTEOS_TOTAL)

#este script como resultado un objeto el cual contiene una lista de tres tablas donde esta la abundancia, dencidad por radio definido y de doble banda por cada
#especie

print("\n\n
      Abundancia de especies encontradas dentro del inventario
      ")

print(ABUND_DENC_ESPECIES[[1]])

print("\n\n
      Densidad de especies encontradas dentro del inventario por metodo de doble banda
      ")

print(ABUND_DENC_ESPECIES[[2]])

print("\n\n
      Densidad de especies encontradas dentro del inventario por metodo radio definido
      ")

print(ABUND_DENC_ESPECIES[[3]])


#......................................................................................................
#abundancia y dencidad por punto de observacion

source("SCRIPS/ABUNDANCIA_DENSIDAD_PUNTO_OBSERVACION.R")

ABUND_DENC_PUNTO_OBSERVACION<-ABUNDANCIA_DENCIDAD_PUNTO_OBSERVACION(PUNTO_DE_OBSERVACION = DATOS_BASE$PUNTO.DE.OBSERVACION,TOTAL_AVISTAMIENTOS = DATOS_BASE$NUMERO.DE.AVISTAMIENTO,AVISTAMIENTOS_DENTRO_DEL_RADIO = DATOS_BASE$DENTRO.DEL.RADIO,AVISTAMIENTOS_FUERA_DEL_RADIO = DATOS_BASE$FUERA.DEL.RADIO,SUPERFICIE = superfice_muestreo,CONTEOS = CONTEOS_TOTAL)


print("\n\n
      Abundancia de individuos encontradas en los puntos de observacion dentro del inventario
      ")

print(ABUND_DENC_PUNTO_OBSERVACION[[1]])

print("\n\n
      Densidad de individuos encontradas en los puntos de observacion dentro del inventario por metodo de radio definido
      ")

print(ABUND_DENC_PUNTO_OBSERVACION[[2]])

print("\n\n
      Densidad de individuos encontradas en los puntos de observacion dentro del inventario por metodo de doble banda
      ")

print(ABUND_DENC_PUNTO_OBSERVACION[[3]])

#GRAFICO DE ABUNDANCIA Y DENSIDAD

#.....................................................................................................

#Se utilizo la libreria patchwork para la generacion de graficos por separado y posterior unificafcion en un solo grafico
#segun si se realizo por punto de observacion o por especie

#grafico por especie

  #grafico de abundancia
  G1<-data.frame(ABUND_DENC_ESPECIES[[1]])  #acignacion de datos para generacion de grafica
  
  G1<-ggplot(G1)+
    geom_col(aes(x=ESPECIE,y=ABUNDANCIA),fill="green",color="gray")+
    ggtitle("ABUNDANCIA POR ESPECIE")+
    #coord_flip()+# si exiten demasiadas especies utilice esta funcion 
    geom_text(aes(label=ABUNDANCIA,x=ESPECIE,y=ABUNDANCIA),size=3)+
    labs(x="ESPECIES",y="ABUNDANCIA")+
    theme(
      title = element_text(size = 10,face="bold.italic",color = "green"),
      axis.title = element_text(size = 8,face = "bold"),
      axis.text = element_text(size = 5,face="italic"),
      axis.text.x = element_text(angle = 60)
      )
  
  #grafico de de densidad por metodo de doble banda
  G2<-data.frame(ABUND_DENC_ESPECIES[[2]])  #acignacion de datos para generacion de grafica
  
  G2<-ggplot(G2)+
    geom_col(aes(x=NOMBRE.CIENTIFICO,y=DENSIDAD.DOBLE.BANDA),fill="red",color="gray")+
    ggtitle("DENSIDAD POR DOBLE BANDA\nPOR ESPECIES")+
    coord_flip()+ 
    geom_text(aes(label=round(DENSIDAD.DOBLE.BANDA,2),x=NOMBRE.CIENTIFICO,y=DENSIDAD.DOBLE.BANDA),size=3)+
    labs(x="ESPECIES",y="DENSIDAD")+
    theme(
      title = element_text(size = 10,face="bold.italic",color = "red"),
      axis.title = element_text(size = 8,face = "bold"),
      axis.text = element_text(size = 5,face="italic")
    )
  
  
  #grafico de de densidad por metodo de radio definido
  G3<-data.frame(ABUND_DENC_ESPECIES[[3]])  #acignacion de datos para generacion de grafica
  
  G3<-ggplot(G3)+
    geom_col(aes(x=NOMBRE.CIENTIFICO,y=DENSIDAD.RADIO.DEFINIDO),fill="blue",color="gray")+
    ggtitle("DENSIDAD POR RADIO DEFINIDO\nPOR ESPECIE")+
    coord_flip()+ 
    geom_text(aes(label=round(DENSIDAD.RADIO.DEFINIDO,2),x=NOMBRE.CIENTIFICO,y=DENSIDAD.RADIO.DEFINIDO),size=3)+
    labs(x="ESPECIES",y="DENSIDAD")+
    theme(
      title = element_text(size = 10,face="bold.italic",color = "blue"),
      axis.title = element_text(size = 8,face = "bold"),
      axis.text = element_text(size = 5,face="italic")
    )
  
      #union de los graficos  
      grafico_dens_abun_especies=G1/(G2|G3)
      
      grafico_dens_abun_especies
      
      

#grafico por punto de observacion
      
  #grafico de abundancia
  G1<-data.frame(ABUND_DENC_PUNTO_OBSERVACION[[1]])  #acignacion de datos para generacion de grafica
      
  G1<-ggplot(G1)+
    geom_col(aes(x=PUNTO.DE.OBSERVACION,y=ABUNDANCIA),fill="green",color="gray")+
    ggtitle("ABUNDANCIA POR PUNTO DE OBS.")+
    #coord_flip()+# si exiten demasiadas especies utilice esta funcion 
    geom_text(aes(label=ABUNDANCIA,x=PUNTO.DE.OBSERVACION,y=ABUNDANCIA),size=3)+
    labs(x="PUNTO DE OBSERVACION",y="ABUNDANCIA")+
    theme(
      title = element_text(size = 10,face="bold.italic",color = "green"),
      axis.title = element_text(size = 8,face = "bold"),
      axis.text = element_text(size = 5,face="italic"),
      axis.text.x = element_text(angle = 60)
      )
  
  #grafico de de densidad por metodo de doble banda
  G2<-data.frame(ABUND_DENC_PUNTO_OBSERVACION[[3]])  #acignacion de datos para generacion de grafica
  
  G2<-ggplot(G2)+
    geom_col(aes(x=PUNTO.DE.OBSERVACION,y=DENSIDAD.DOBLE.BANDAD),fill="red",color="gray")+
    ggtitle("DENSIDAD POR DOBLE BANDA\nPOR PUNTO DE OBS.")+
    coord_flip()+ 
    geom_text(aes(label=round(DENSIDAD.DOBLE.BANDAD,2),x=PUNTO.DE.OBSERVACION,y=DENSIDAD.DOBLE.BANDAD),size=3)+
    labs(x="PUNTO DE OBSERVACION",y="DENSIDAD")+
    theme(
      title = element_text(size = 10,face="bold.italic",color = "red"),
      axis.title = element_text(size = 8,face = "bold"),
      axis.text = element_text(size = 5,face="italic")
      )
      
  #grafico de de densidad por metodo de radio definido
  G3<-data.frame(ABUND_DENC_PUNTO_OBSERVACION[[2]])  #acignacion de datos para generacion de grafica
      
  G3<-ggplot(G3)+
    geom_col(aes(x=PUNTO.DE.OBSERVACION,y=DENSIDAD.RADIO.DEFINIDO),fill="blue",color="gray")+
    ggtitle("DENSIDAD POR RADIO DEFINIDO\nPOR PUNTO DE.OBS.")+
    coord_flip()+ 
    geom_text(aes(label=round(DENSIDAD.RADIO.DEFINIDO,2),x=PUNTO.DE.OBSERVACION,y=DENSIDAD.RADIO.DEFINIDO),size=3)+      labs(x="PUNTO DE OBSERVACION",y="DENSIDAD")+
    theme(
      title = element_text(size = 10,face="bold.italic",color = "blue"),
      axis.title = element_text(size = 8,face = "bold"),
      axis.text = element_text(size = 5,face="italic")
        )
  
  #unificacion de grafica y presentacion
  grafico_dens_abun_punto_obs=G1/(G2|G3)
  
  grafico_dens_abun_punto_obs

  
#GRAFICO DE FRECUENCIA DE AVISTAMIENTOS POR MES-------------------------------------------------------------------------------------------------------------------
  lista_grafico_tiempo_observaciones<-GRAFICO_DE_MESES_MAYOR_OBSERVACION(ESPECIES = DATOS_BASE$NOMBRE.CIENTIFICO,FECHA = DATOS_BASE$FECHA,OBSERVACIONES = DATOS_BASE$NUMERO.DE.AVISTAMIENTO)
  
  
  lista_grafico_tiempo_observaciones
  
  observaciones_mes_especie<-lista_grafico_tiempo_observaciones[[1]][["data"]]
  observaciones_mes_especie<-transformacion_de_datos(filas = observaciones_mes_especie$ESPECIES,columnas = observaciones_mes_especie$FECHA,valor = observaciones_mes_especie$OBSERVACIONES)
  
  observaciones_mes<-lista_grafico_tiempo_observaciones[[2]][["data"]]

print("
      tabla de resumen de observaciones de espcies por mes dentro del inventario
      ")
print(observaciones_mes_especie)  

print("
      tabla de resumen de observaciones por mes dentro del inventario
      ")
print(observaciones_mes)


#CURVAS DE ACUMULACION---------------------------------------------------------------------------------

source("SCRIPS/CURVA_ACUMULACION.R")

"
el analisis para curvas de acumulacion y raferacion se realizara no por el numero de parcelas, si no por las observaciones realizadas
en la realizacion del inventario de ornitofauna, si se desea trabajar con solo los puntos de  observacion o parcelas
elimine la proxima linea de codigo
"
FORMATO_DIVERSIDAD<-transformacion_de_datos(filas = DATOS_BASE$OBSERVACIONES,columnas = DATOS_BASE$NOMBRE.CIENTIFICO,valor = DATOS_BASE$NUMERO.DE.AVISTAMIENTO)
  
curvas_acumulacion<-modelos_de_curvas_de_acumulacion(FORMATO_DIVERSIDAD_VEGAN = FORMATO_DIVERSIDAD,PERMUTACIONES = 100)


  #grafico de acumulkacion de especies
  #...................................................................................................
      #pre prosesamiento de datos para grafica

          datos_grafico<-data.frame(Metodo="Randon",Estimado=curvas_acumulacion$RANDON_ESTIMADOR,Observaciones=curvas_acumulacion$RANDON_SITIOS,
                                    Desv.Est.=curvas_acumulacion$RANDON_DESV_EST,Sitios=curvas_acumulacion$ACUMULACION_SITIOS,Especies=curvas_acumulacion$ACUMULACION)
          
          auxiliar<-data.frame(Metodo="Exact",Estimado=curvas_acumulacion$EXACT_ESTIMADOR,Observaciones=curvas_acumulacion$EXACT_SITIOS,
                          Desv.Est.=curvas_acumulacion$EXACT_DESV_EST,Sitios=curvas_acumulacion$ACUMULACION_SITIOS,Especies=curvas_acumulacion$ACUMULACION)

          datos_grafico<-rbind(datos_grafico,auxiliar)

          auxiliar<-data.frame(Metodo="Coleman",Estimado=curvas_acumulacion$COLEMAN_ESTIMADOR,Observaciones=curvas_acumulacion$COLEMAN_SITIOS,
                               Desv.Est.=curvas_acumulacion$COLEMAN_DESV_EST,Sitios=curvas_acumulacion$ACUMULACION_SITIOS,Especies=curvas_acumulacion$ACUMULACION)

          datos_grafico<-rbind(datos_grafico,auxiliar)

          auxiliar<-data.frame(Metodo="Rarefación",Estimado=curvas_acumulacion$RAREFACCION_ESTIMADOR,Observaciones=curvas_acumulacion$RAREFACCION_SITIOS,
                               Desv.Est.=curvas_acumulacion$RAREFACCION_DESV_EST,Sitios=curvas_acumulacion$ACUMULACION_SITIOS,Especies=curvas_acumulacion$ACUMULACION)

          datos_grafico<-rbind(datos_grafico,auxiliar)

      #generacion del grafico
          
ggplot(datos_grafico,aes(x=Observaciones,y=Estimado,ymax=Estimado+(Desv.Est.*1.96),ymin=Estimado-(Desv.Est.*1.96),color=Metodo))+
  geom_ribbon(aes(alpha=95),fill=8,color=0)+
  #geom_point()+
  geom_line(lty=1)+
  #geom_linerange()+
  facet_wrap(~Metodo)+
  geom_line(aes(x=Sitios,y=Especies),linewidth=1,lty=3,color="black")+
  labs(line = "Título")+
  ggtitle("MODELOS DE CURVAS DE ACUMULACION")+
  labs(x="OBSERVACIONES",y="ESPECIES")+
  theme(
    panel.background =element_rect(fill = "white"),
    panel.border = element_rect(fill="transparent",color="black",size = 1),
    panel.grid = element_line(color = "gray",size=0.5,linetype = 3),
    axis.title = element_text(size=10),
    axis.text = element_text(size=7,face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(size=12),
    legend.text = element_text(size = 9),
    legend.background = element_rect(fill="transparent",color = "black"),
    title = element_text(size=14),
    strip.text = element_text(size=12,face = "bold",color = "white"),
    strip.background=element_rect(fill = "dodgerblue1",linetype = "solid",color = "black",linewidth = 1)
  )
  
#cambio de nombre de variables de tanla de acumulaciond e especies

colnames(curvas_acumulacion)<-c("Observaciones Realizadas","Especies Descubiertas","Sitios Est. RANDON","Estimacion de Esp. RANDON",
                                "Desviacion Estandar Randon","Sitios Est. ExaCT","Estimacion de Esp. EXACT","Desviacion Estandar EXACT",
                                "Sitios Est. COLEMAN","Estimacion de esp. COLEMAN","Desviacion Estandar COLEMAN","sitios Est. RAREFACCION",
                                "Estimacion de Esp. RAREFACCION","Desviacion Estandar RAREFACCION")


#RAREFACION------------------------------------------------------------------------------------------------------------------------

#RAREFACCION POR ESPECIE 
#..................................................................................................................................

total_avistamientos<-sum(FORMATO_DIVERSIDAD)

vector_inter<-c(seq(20,floor(total_avistamientos/20)*20,by=20),total_avistamientos)

RAREFACCION_ESP<-data.frame(t(rarefy(colSums(FORMATO_DIVERSIDAD),vector_inter,se = T)),vector_inter)

colnames(RAREFACCION_ESP)<-c("NUMERÓ ESPECIES","ERROR ESTANDAR","NUMERO DE INDIVIDUOS")

  #GRAFICO DE RAREFACCION POR ESPECIE
  G1<-data.frame(x=RAREFACCION_ESP$`NUMERO DE INDIVIDUOS`,y=RAREFACCION_ESP$`NUMERÓ ESPECIES`,sd=RAREFACCION_ESP$`ERROR ESTANDAR`)
  
  #generacion del grafico
  
  grafico_rarefaccion_especie<-ggplot(G1,aes(x=x,y=y,ymax=y+(sd*1.96),ymin=y-(sd*1.96)))+
    geom_ribbon(aes(alpha=95),fill="skyblue",color="skyblue")+
    geom_line(lty=1)+
    ggtitle("RAREFACCION POR AVISTAMIENTOS")+
    labs(x="AVISTAMIENTOS",y="ESPECIES")+
    theme(
      panel.background =element_rect(fill = "white"),
      panel.border = element_rect(fill="transparent",color="black",size = 1),
      panel.grid = element_line(color = "gray",size=0.5,linetype = 3),
      axis.title = element_text(size=10),
      axis.text = element_text(size=7,face = "bold"),
      legend.title = element_text(size=10),
      legend.text = element_text(size = 9),
      legend.background = element_rect(fill="transparent",color = "black"),
      title = element_text(size=14)
      )

  grafico_rarefaccion_especie



#MODELOS PARA ESTIMACION DE RIQUEZA--------
#...................................................................................................

#MODELOS PARAMETRICOS PARA ESTIMACION DE RIQUEZA
  
MODELO_ESTIMADOR_PARAMETRICO<-estaccumR(FORMATO_DIVERSIDAD,permutations = 100)

ESTIMADOR_PARAMETRICO<-data.frame(MODELO_ESTIMADOR_PARAMETRICO[["means"]])

    #grafica de estimador de riqueza con metodos parametricos
    
    #PREPARACION DE DATOS PARA GENERACION DE UN GRAFICO
    datos_grafico<-data.frame(MODELO="S",ENCUENTROS=ESTIMADOR_PARAMETRICO$N,ESTIMADOR=ESTIMADOR_PARAMETRICO$S,MAX_Y_MIN(MODELO_ESTIMADOR_PARAMETRICO$S))
    
    auxiliar<-data.frame(MODELO="CHAO 1",ENCUENTROS=ESTIMADOR_PARAMETRICO$N,ESTIMADOR=ESTIMADOR_PARAMETRICO$Chao,MAX_Y_MIN(MODELO_ESTIMADOR_PARAMETRICO$chao))
    datos_grafico<-rbind(datos_grafico,auxiliar)
    
    auxiliar<-data.frame(MODELO="ACE",ENCUENTROS=ESTIMADOR_PARAMETRICO$N,ESTIMADOR=ESTIMADOR_PARAMETRICO$ACE,MAX_Y_MIN(MODELO_ESTIMADOR_PARAMETRICO$ace))
    datos_grafico<-rbind(datos_grafico,auxiliar)
    
      #GRAFICA
    GRAFICO_MODELO_ESTIMADOR_PARAMETRICO<-ggplot(datos_grafico,aes(x=ENCUENTROS,y=ESTIMADOR,ymax=MAXIMO,ymin=MINIMO,color=MODELO))+
      geom_ribbon(fill=8,color=0)+
      geom_line(lty=1)+
      facet_wrap(~MODELO)+
      labs(line = "Título")+
      ggtitle("MODELOS DE ESTIMADORES DE RIQUEZA PARAMETRICOS")+
      labs(x="ENCUENTROS",y="ESPECIES")+
      theme(
        panel.background =element_rect(fill = "white"),
        panel.border = element_rect(fill="transparent",color="black",size = 1),
        panel.grid = element_line(color = "gray",size=0.5,linetype = 3),
        axis.title = element_text(size=10),
        axis.text = element_text(size=7,face = "bold"),
        legend.position = "bottom",
        legend.title = element_text(size=12),
        legend.text = element_text(size = 9),
        legend.background = element_rect(fill="transparent",color = "black"),
        title = element_text(size=14),
        strip.text = element_text(size=12,face = "bold",color = "white"),
        strip.background=element_rect(fill = "dodgerblue1",linetype = "solid",color = "black",linewidth = 1)
        )
    
    GRAFICO_MODELO_ESTIMADOR_PARAMETRICO
    
    MODELO_ESTIMADOR_PARAMETRICO_PARA_RIQUEZA=estimateR(colSums(FORMATO_DIVERSIDAD))
    
    #ESTIMADOR DE RIQUEZA PARA NUMERO DE ESPECIES POSIBLES 
    
    print("
          \n\n\nestimador de riqueza por modelos parametricos\n\n
          ")
    print(MODELO_ESTIMADOR_PARAMETRICO_PARA_RIQUEZA)
    
    
#MODELOS NO PARAMETRICOS PARA ESTIMACION DE RIQUEZA

MODELO_ESTIMADOR_NO_PARAMETRICO<-poolaccum(FORMATO_DIVERSIDAD,permutations = 100)

MODELO_ESTIMADOR_NO_PARAMETRICO_GRAFRICO<-data.frame(MODELO_ESTIMADOR_NO_PARAMETRICO[["means"]])

  #grafica de estimador de riqueza con metodos parametricos

    #PREPARACION DE DATOS PARA GENERACION DE UN GRAFICO
    datos_grafico<-data.frame(MODELO="S",ENCUENTROS=MODELO_ESTIMADOR_NO_PARAMETRICO_GRAFRICO$N,ESTIMADOR=MODELO_ESTIMADOR_NO_PARAMETRICO_GRAFRICO$S,MAX_Y_MIN(MODELO_ESTIMADOR_NO_PARAMETRICO$S))
    
    auxiliar<-data.frame(MODELO="CHAO 2",ENCUENTROS=MODELO_ESTIMADOR_NO_PARAMETRICO_GRAFRICO$N,ESTIMADOR=MODELO_ESTIMADOR_NO_PARAMETRICO_GRAFRICO$Chao,MAX_Y_MIN(MODELO_ESTIMADOR_NO_PARAMETRICO$chao))
    datos_grafico<-rbind(datos_grafico,auxiliar)
    
    auxiliar<-data.frame(MODELO="JACKKNIFE 1",ENCUENTROS=MODELO_ESTIMADOR_NO_PARAMETRICO_GRAFRICO$N,ESTIMADOR=MODELO_ESTIMADOR_NO_PARAMETRICO_GRAFRICO$Jackknife.1,MAX_Y_MIN(MODELO_ESTIMADOR_NO_PARAMETRICO$jack1))
    datos_grafico<-rbind(datos_grafico,auxiliar)
    
    auxiliar<-data.frame(MODELO="JACKKNIFE 2",ENCUENTROS=MODELO_ESTIMADOR_NO_PARAMETRICO_GRAFRICO$N,ESTIMADOR=MODELO_ESTIMADOR_NO_PARAMETRICO_GRAFRICO$Jackknife.2,MAX_Y_MIN(MODELO_ESTIMADOR_NO_PARAMETRICO$jack2))
    datos_grafico<-rbind(datos_grafico,auxiliar)
    
    auxiliar<-data.frame(MODELO="BOOTSTRAP",ENCUENTROS=MODELO_ESTIMADOR_NO_PARAMETRICO_GRAFRICO$N,ESTIMADOR=MODELO_ESTIMADOR_NO_PARAMETRICO_GRAFRICO$Bootstrap,MAX_Y_MIN(MODELO_ESTIMADOR_NO_PARAMETRICO$boot))
    datos_grafico<-rbind(datos_grafico,auxiliar)
    
    
    #GRAFICA
    GRAFICO_MODELO_ESTIMADOR_NO_PARAMETRICO<-ggplot(datos_grafico,aes(x=ENCUENTROS,y=ESTIMADOR,ymax=MAXIMO,ymin=MINIMO,color=MODELO))+
      geom_ribbon(fill=8,color=0)+
      geom_line(lty=1)+
      facet_wrap(~MODELO)+
      labs(line = "Título")+
      ggtitle("MODELOS DE ESTIMADORES DE RIQUEZA NO PARAMETRICOS")+
      labs(x="ENCUENTROS",y="ESPECIES")+
      theme(
        panel.background =element_rect(fill = "white"),
        panel.border = element_rect(fill="transparent",color="black",size = 1),
        panel.grid = element_line(color = "gray",size=0.5,linetype = 3),
        axis.title = element_text(size=10),
        axis.text = element_text(size=7,face = "bold"),
        legend.position = "bottom",
        legend.title = element_text(size=12),
        legend.text = element_text(size = 9),
        legend.background = element_rect(fill="transparent",color = "black"),
        title = element_text(size=14),
        strip.text = element_text(size=12,face = "bold",color = "white"),
        strip.background=element_rect(fill = "dodgerblue1",linetype = "solid",color = "black",linewidth = 1)
        )
    
    GRAFICO_MODELO_ESTIMADOR_NO_PARAMETRICO
    
    #ESTIMADOR DE RIQUEZA PARA NUMERO DE ESPECIES POSIBLES
    
    MODELO_ESTIMADOR_NO_PARAMETRICO_PARA_RIQUEZA=specpool(FORMATO_DIVERSIDAD)

    #ESTIMADOR DE RIQUEZA PARA NUMERO DE ESPECIES POSIBLES 
    
    print("
          \n\n\nestimador de riqueza por modelos no parametricos\n\n
          ")
    print(MODELO_ESTIMADOR_NO_PARAMETRICO_PARA_RIQUEZA)
    
    