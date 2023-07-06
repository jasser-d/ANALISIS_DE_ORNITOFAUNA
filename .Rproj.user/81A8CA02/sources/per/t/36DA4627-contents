"Este script esta diseñado para la ejecucion de la tesis de ´Analisis de ornitofauna de las zonas antropicas de la
de la FACAP´ en donde se determinara densidad, abundacia, indices de diversidad, curvas de acumulacion y raferacion.
Ademas de los meses de mayor avistamiento por cada especie, distribuvion biogeografica con modelos de EBIRD y el nivel
de conservacion de la UNIC y el libro rojo de Ecuador"
# librerias nesesarias -------------------------------------------------------------------
'install.packages("renv")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("vegan")
install.packages("plotrix")
install.packages("patchwork")
install.packages("ggwordcloud")
install.packages("extrafont")
install.packages("showtext")
install.packages("ggthemes")
install.packages("openxlsx2")'

# preambulo de carga de datos --------------------------------------------------------
rm(list = ls())

#librerias activar
library(renv)
library(tidyverse)
library(ggplot2)
library(vegan)
library(plotrix)
library(patchwork)
library(ggwordcloud)
library(ggthemes)
library(broom)
library(openxlsx2)
library(extrafont)
library(showtext)
#font_import()#si se desea importar fuentes no integradas en r
loadfonts(device = "win")

#archivos necesarios---------------------------------------------------------------

print("Este script necesita los sigientes datos para porder operrar
      -base de datos en formato .csv separado por comas
      -radio de los puntos de observacion en metros
      -si desea cambiar la ruta de defecto de almacenamiento de los resultado, lo puede cambiar
      puede cambiarlos dentro de los proximos bloques de codigo"
      )
ruta_de_archivos<-"DATOS_BASE.csv"   #escriba la ruta de archivos o el nombre del mismo
area<-pi * 20^2   #escriba el area de los puntos de observacion
ruta_informacion_generada<-"INFROMACION_GENERADA/"#son pruebas de hipotesis y varias tablas
ruta_graficos<-"GRAFICAS/"# graficas generadas


#cargar archivos

df_base<-read.csv(file = ruta_de_archivos,sep = ";")

#carga scripsts--------------------------------------------------------------------

#ambundancia, densidad por los dos metodos doble banda y radio definido

source("SCRIPS/ABUNDANCIA_DENSIDAD_ESPECIE.R")#PARA CADA ESPECIE
source("SCRIPS/ABUNDANCIA_DENSIDAD_PUNTO_OBSERVACION.R")#para cada punto de observacion

#transdormador de los datos para uso de la libreria vegan
source("SCRIPS/TRANSFORMACION.R")

#diversiad alfa
source("SCRIPS/DIVERSIDAD_ALFA.R")

#exportar resultados en un rachivo de excel 
  excel_guradado<-function(tabla,ruta,nombre){
    write_xlsx(x = tabla, file = paste0(ruta, nombre, ".xlsx"),sheetName=c(names(tabla)),
               gridLines=F)
  }
  
#exportar graficas
  exportador_graficas<-function(ruta,nombre,grafica,alto_cm=10,ancho_cm=14){
    png(filename = paste0(ruta, nombre, ".png"),width = ancho_cm,height = alto_cm,res= 300,units = "cm")
    print(grafica)
    dev.off()
  }

  #generador de graficos parar densidad, poblacion y diversidad
  
  generador_grafica<-function(tabla_grafia,vertical=T,nombre,ejes=c("Especies","Dencidad")){
    tabla<-data.frame(x=tabla_grafia[,1],y=tabla_grafia[,2])
    
    grafica<-ggplot(tabla,aes(x=x,y=y,fill="skyblue"))+
      geom_col(linewidth=0.2,colour="white")+
      ggtitle(nombre)+
      labs(x=ejes[1],y=ejes[2])+
      theme_calc() +
      scale_fill_calc() +
      theme(
        text = element_text(family = "Times New Roman"),
        plot.title = element_text(size = 12,face = "bold",vjust = 0.5,hjust = 0.5),
        axis.title = element_text(face = "italic", size = 8),
        axis.text = element_text(face = "bold", size = 6,hjust = 1),
        legend.position = "none")
    
    if (vertical) {
      grafica=grafica+coord_flip()
    }else{
      grafica=grafica+theme(
        axis.text.x = element_text(angle = 90)
      )
    }
    
    return(grafica)
  }


#analisis estadistico-------------------------------------------------------------

avistamiento_punto <- df_base %>% group_by (PUNTO.DE.OBSERVACION) %>% summarise(Avistamientos= sum(NUMERO.DE.AVISTAMIENTO),asTable=T)

#histograma

breaks <- pretty(range(avistamiento_punto$Avistamientos),
                 n = nclass.Sturges(avistamiento_punto$Avistamientos),
                 min.n = 1)

histograma<-ggplot(avistamiento_punto,aes(x=Avistamientos))+
  geom_histogram(color = 1, fill = "green",breaks=breaks)+
  geom_vline(xintercept = mean(avistamiento_punto$Avistamientos),
             linetype = 2,
             color = 2)+
  labs(x="",y="observaciones")+
  ggtitle("Avistamientos por punto de observacion")+
  theme_calc() +
  scale_fill_calc() +
  theme(
    text = element_text(family = "Times New Roman"),
    plot.title = element_text(size = 12,face = "bold",vjust = 0.5,hjust = 0.5),
    axis.title = element_text(size=9,face = "italic")
    )
print(histograma)
exportador_graficas(grafica = histograma,ruta = ruta_graficos,nombre = "histograma")


#grafico de cajas

cajas_g<-ggplot(avistamiento_punto,aes(x=Avistamientos))+
  geom_boxplot(color=1,fill="skyblue")+
  coord_flip ()+
  theme_calc() +
  scale_fill_calc() +
  ggtitle("Grafica de cajas para puntos de observacion")+
  theme(
    text = element_text(family = "Times New Roman"),
    plot.title = element_text(size = 12,face = "bold",vjust = 0.5,hjust = 0.5),
    axis.title = element_text(size=9,face = "italic")
  )
print(cajas_g)
exportador_graficas(ruta =ruta_graficos,nombre = "grafico de cajas",grafica = cajas_g)

#grafico de cuartines
qq_grafica<-ggplot(avistamiento_punto, aes(sample=Avistamientos)) +
  stat_qq(color="green",size=2) + 
  stat_qq_line(color="blue",lty=2)+
  labs(x="Cuartiles teoricos",y="Avistamientos")+
  ggtitle("Q-Q para Avistamientos")+
  theme_calc() +
  scale_fill_calc() +
  theme(
    text = element_text(family = "Times New Roman"),
    plot.title = element_text(size = 12,face = "bold",vjust = 0.5,hjust = 0.5),
    axis.title = element_text(size=9,face = "italic")
  )
print(qq_grafica)
exportador_graficas(ruta = ruta_graficos,nombre = "q_q_grafica",grafica = qq_grafica)


#pruebas de normalidad
komoroft_test<-ks.test(avistamiento_punto$Avistamientos,"pnorm")#prueba de komoroft smirnov
print(komoroft_test)
komoroft_test<-t(tidy(komoroft_test))
komoroft_test<-cbind(analisis=rownames(komoroft_test),estadisticos=komoroft_test[,1])

shapiro_wilk<-shapiro.test(x = avistamiento_punto$Avistamientos)#prueba shapiro wilk
print(shapiro_wilk)
shapiro_wilk<-t(tidy(shapiro_wilk))
shapiro_wilk<-cbind(analisis=rownames(komoroft_test),estadisticos=shapiro_wilk[,1])

intervalo_conf<-qt(df = length(avistamiento_punto$Avistamientos),p = 0.975)*sd(avistamiento_punto$Avistamientos)/sqrt(length(avistamiento_punto$Avistamientos))
lmax=mean(avistamiento_punto$Avistamientos+intervalo_conf)
lmin=mean(avistamiento_punto$Avistamientos-intervalo_conf)

Error_muestral<-(intervalo_conf/mean(avistamiento_punto$Avistamientos))*100

estadistica_descriptiva<-avistamiento_punto%>%summarise(Media=mean(Avistamientos),Mediana=median(Avistamientos),'Desviacion Estandar'=sd(Avistamientos),
                                                        Varianza=var(Avistamientos),'Coeficiente de variacion'=(sd(Avistamientos)/mean(Avistamientos)))%>%t()
estadistica_descriptiva<-rbind(estadistica_descriptiva,"Intervalo de confianza"=intervalo_conf,"limite maximo"=lmax,"liminte minimo"=lmin,
                               "Error muestral"=Error_muestral)
estadistica_descriptiva<-cbind(Estadistica=row.names(estadistica_descriptiva),Valor=estadistica_descriptiva[,1])

estadsiticos<-list(komoroft_test,shapiro_wilk,estadistica_descriptiva)

names(estadsiticos)<-c("KOMOROFT TEST","SHAPIRO TEST","Estadistica Descriptiva")

excel_guradado(tabla = estadsiticos,ruta = ruta_informacion_generada,nombre = "analisis estadistico")

#limpiar entorno
rm(avistamiento_punto,breaks,cajas_g,Error_muestral,estadistica_descriptiva,estadsiticos,histograma,intervalo_conf,komoroft_test,lmax,lmin,qq_grafica,shapiro_wilk)

#abundacia y densidad por especie -------------------------------------------------------------------------------------------------------------

#numero de conteos realizados

conteos<-unique(df_base$OBSERVACIONES)%>%length()%>%as.numeric()

DENSIDAD_ABUNDANCIA_ESPECIE<-ABUNDANCIA_DENCIDAD_ESPECIES(NOMBRE_CIENTIFICO = df_base$NOMBRE.CIENTIFICO,TOTAL_AVISTAMIENTOS = df_base$NUMERO.DE.AVISTAMIENTO,
                          AVISTAMIENTOS_DENTRO_DEL_RADIO = df_base$DENTRO.DEL.RADIO,AVISTAMIENTOS_FUERA_DEL_RADIO = df_base$FUERA.DEL.RADIO,SUPERFICIE = area,
                          CONTEOS = conteos)

DENSIDAD_ABUNDANCIA_PUNTO<-ABUNDANCIA_DENCIDAD_PUNTO_OBSERVACION(PUNTO_DE_OBSERVACION = df_base$PUNTO.DE.OBSERVACION,TOTAL_AVISTAMIENTOS = df_base$NUMERO.DE.AVISTAMIENTO,
                          AVISTAMIENTOS_DENTRO_DEL_RADIO = df_base$DENTRO.DEL.RADIO,AVISTAMIENTOS_FUERA_DEL_RADIO = df_base$FUERA.DEL.RADIO,SUPERFICIE = area,CONTEOS = conteos)

DENSIDAD_ABUNDANCIA<-list(DENSIDAD_ABUNDANCIA_ESPECIE$ABUNDACIA,DENSIDAD_ABUNDANCIA_ESPECIE$DENSIDAD_DOBLE_BANDA,DENSIDAD_ABUNDANCIA_ESPECIE$DENSIDAD_RADIO_DEFINIDO,
                          DENSIDAD_ABUNDANCIA_PUNTO$ABUNDANCIA,DENSIDAD_ABUNDANCIA_PUNTO$DENSIDAD_DOBLE_BANDA,DENSIDAD_ABUNDANCIA_PUNTO$DENSIDAD_RADIO_DEFINIDO)
names(DENSIDAD_ABUNDANCIA)<-c("ABUNDACIA ESP","DENSIDAD_DOBLE_BANDA ESP","DENSIDAD_RADIO_DEFINIDO ESP","ABUNDACIA PUN","DENSIDAD_DOBLE_BANDA PUN","DENSIDAD_RADIO_DEFINIDO PUN")

excel_guradado(tabla = DENSIDAD_ABUNDANCIA,ruta = ruta_informacion_generada,nombre = "dencidad y abundancia")



nombres<-c("ABUNDACIA ESPECIES","DENSIDAD DOBLE BANDA POR ESPECIE","DENSIDAD DE RADIO DEFINIDO POR ESPECIE","ABUNDACIA PUNTO DE OBS.","DENSIDAD DE DOBLE BANDA PUNTO DE OBS.","DENSIDAD RADIO DEFINIDO PUNTO DE OBS.")

for (i in 1:6) {
  if (i==1){
    ejes<-c("Especies","Abundancia")
  }else if(i<=4){
    ejes<-c("Especies","Dencidad N/has")
  }else if(i==4){
    ejes<-c("Puntos de observacion","Abundancia")
  }else{
    ejes<-c("Puntos de observacion","Dencidad N/has")
  }
  grafica<-generador_grafica(tabla_grafia = DENSIDAD_ABUNDANCIA[[i]],vertical = F,nombre = nombres[i],ejes =ejes)
  print(grafica)
  exportador_graficas(grafica = grafica,ruta = ruta_graficos,nombre = nombres[i],alto_cm = 7,ancho_cm = 15)
}

#limpieza de entorno
rm( ABUNDANCIA_DENCIDAD_ESPECIES,ABUNDANCIA_DENCIDAD_PUNTO_OBSERVACION,DENSIDAD_ABUNDANCIA,DENSIDAD_ABUNDANCIA_PUNTO,
    ejes,grafica,i,nombres,DENSIDAD_ABUNDANCIA_ESPECIE)

#diversidad-----------------------------------------------------------------------------------------------------------

#el inice de diversidad se trabajar en funcion del los puntos de observacion y general

formato_vegan_parcelas<-transformacion_de_datos(filas = df_base$PUNTO.DE.OBSERVACION,columnas = df_base$NOMBRE.CIENTIFICO,valor = df_base$NUMERO.DE.AVISTAMIENTO)

indices_diversidad<-DIVERSIDAD_ALFA(base_datos = formato_vegan_parcelas)

excel_guradado(tabla = indices_diversidad,ruta = ruta_informacion_generada,nombre = "indices de diversidad")

g1<-generador_grafica(tabla_grafia = data.frame(indices_diversidad$PARCELAS$PARCELAS,as.numeric(indices_diversidad$PARCELAS$SIMPSOM)),
                  vertical = F,nombre = "DIVERSIDAD DE\nSIMPSOM",ejes = c("PARCELAS","INDICE"))
g2<-generador_grafica(tabla_grafia = data.frame(indices_diversidad$PARCELAS$PARCELAS,as.numeric(indices_diversidad$PARCELAS$SHANNOM)),
                      vertical = F,nombre = "DIVERSIDAD DE\nSHANNOM",ejes = c("PARCELAS","INDICE"))
g3<-generador_grafica(tabla_grafia = data.frame(indices_diversidad$PARCELAS$PARCELAS,as.numeric(indices_diversidad$PARCELAS$MENHINICK)),
                      vertical = F,nombre = "DIVERSIDAD DE\nMENHINICK",ejes = c("PARCELAS","INDICE"))
g1<-g1+g2+g3

exportador_graficas(grafica = g1,ruta = ruta_graficos,nombre = "GRAFICAS DE DIVERSIDAD",alto_cm = 8,ancho_cm = 15)

print(g1)

rm(DIVERSIDAD_ALFA,g1,g2,g3,indices_diversidad,formato_vegan_parcelas)

#curvas de acumulacion de especies---------------------------------------------------------------------------------------------------------

#en este caso y en raferacion se usara la unidad de trabajo la cual son als observaciones realizadas


formato_diversidad<-transformacion_de_datos(filas = df_base$OBSERVACIONES,columnas = df_base$NOMBRE.CIENTIFICO,valor = df_base$NUMERO.DE.AVISTAMIENTO)

source("SCRIPS/CURVA_ACUMULACION.R")

modelos_curvas<-modelos_de_curvas_de_acumulacion(FORMATO_DIVERSIDAD_VEGAN = formato_diversidad,PERMUTACIONES = 100)

excel_guradado(tabla = modelos_curvas,ruta = ruta_informacion_generada,nombre = "curva de acumulacion de especies")

grafica<-data.frame()

nombres<-c("RANDOM","EXACT","COLEMAN","RAREFACCION")
control<-c(3,6,9,12)

for (i in 1:4) {
  sitios<-modelos_curvas[,control[i]]
  especies<-modelos_curvas[,(control[i]+1)]
  intervalo_conf<-1.96*(modelos_curvas[,(control[i]+2)]/sqrt((modelos_curvas[,(control[i]+1)])))
  
  if (i==1) {
    grafica<-data.frame(MODELO=nombres[1],ACUMULACION_SITIOS=modelos_curvas[1],ACUMULACION=modelos_curvas[2],SITIOS=sitios,ESPECIES=especies,INTER_CONF=intervalo_conf)
  }else{
    BUCLE<-data.frame(MODELO=nombres[i],ACUMULACION_SITIOS=modelos_curvas[1],ACUMULACION=modelos_curvas[2],SITIOS=sitios,ESPECIES=especies,INTER_CONF=intervalo_conf)
    grafica<-rbind(grafica,BUCLE)
  }
}

grafica<-ggplot(grafica,aes(x = SITIOS,y = ESPECIES,color = MODELO,ymax=ESPECIES+INTER_CONF,ymin=ESPECIES-INTER_CONF))+
  geom_ribbon(fill=8,color=0)+
  geom_line()+
  #geom_point()+
  #geom_linerange()+
  geom_line(aes(x = ACUMULACION_SITIOS,y = ACUMULACION,color="Observaciones"),color="black",linewidth=0.25,lty=2)+
  facet_wrap(~MODELO,strip.position = "bottom")+
  ggtitle("CURVAS DE ACUMULACION")+
  scale_fill_calc() +
  theme(
    panel.background = element_rect(fill = "white",colour = "black"),
    panel.grid.major.y = element_line(colour = "#C2C2C2",linewidth = 0.5),
    text = element_text(family = "Times New Roman"),
    legend.position = "bottom",
    plot.title = element_text(size = 10,color = "skyblue",face = "bold",vjust = 0.5,hjust = 0.5),
    axis.title = element_text(size = 6,face = "italic"),
    axis.text = element_text(size = 5,face = "bold"),
    strip.text = element_text(colour = "white",size = 8,face = "italic",hjust = 0.5,vjust = 0.5),
    strip.background = element_rect(fill = "#597CFF",color = "black",linewidth = 0.5)
  )
print(grafica)

exportador_graficas(ruta = ruta_graficos,nombre = "curvas de acumulacion",grafica = grafica,alto_cm = 9,ancho_cm = 15)

#limpieza de entorno
rm(BUCLE,grafica,modelos_curvas,i,control,especies,intervalo_conf,nombres,sitios,modelos_de_curvas_de_acumulacion)

#raferacion-----------------------------------------------------------------------------------------------------------

total_avistamientos<-sum(formato_diversidad)

vector_inter<-c(seq(20,floor(total_avistamientos/20)*20,by=20),total_avistamientos)

RAREFACCION_ESP<-data.frame(t(rarefy(colSums(formato_diversidad),vector_inter,se = T)),vector_inter)
colnames(RAREFACCION_ESP)<-c("NUMERÓ ESPECIES","ERROR ESTANDAR","NUMERO DE INDIVIDUOS")

excel_guradado(tabla = RAREFACCION_ESP,ruta = ruta_informacion_generada,nombre = "curva de rarefaccion 20")

G1<-data.frame(x=RAREFACCION_ESP$`NUMERO DE INDIVIDUOS`,y=RAREFACCION_ESP$`NUMERÓ ESPECIES`,sd=RAREFACCION_ESP$`ERROR ESTANDAR`)

grafica<-ggplot(G1,aes(x = x,y = y,ymax=y+sd,ymin=y-sd))+
  geom_ribbon(fill=8,color=0)+
  geom_line(color = "green")+
  #geom_point()+
  #geom_linerange()+
  ggtitle("RAREFACCION POR ENCUENTRO DE INDIVIDUOS")+
  labs(x="Avistamientos",y="")+
  scale_fill_calc() +
  theme(
    panel.background = element_rect(fill = "white",colour = "black"),
    panel.grid.major.y = element_line(colour = "#C2C2C2",linewidth = 0.5),
    text = element_text(family = "Times New Roman"),
    legend.position = "bottom",
    plot.title = element_text(size = 10,color = "skyblue",face = "bold",vjust = 0.5,hjust = 0.5),
    axis.title = element_text(size = 8,face = "italic"),
    axis.text = element_text(size = 6,face = "bold"),
  )

print(grafica)

exportador_graficas(ruta = ruta_graficos,nombre = "curva de raferacion por avistamiento",grafica = grafica,alto_cm = 7,ancho_cm =13)

#estimadores de riqueza por raferacion-------------------------------------------------------------------------------------

#estimadores parametricos

estimador_parametrico<-estaccumR(x = formato_diversidad,permutations = 100)

nombres_est<-c("S","chao","ace")

tabla_estimadores_paramericos<-NULL

for (i in nombres_est) {
  ayudar<-estimador_parametrico[[i]]
  estadisticos<-NULL
  for (e in 1:dim(ayudar)[1]) {
    fila<-ayudar[e,]
    if (e==1) {
      estadisticos<-data.frame(MAXIMO=max(fila),MINIMO=min(fila),MEDIA=mean(fila))
    }else{
      calculo<-data.frame(MAXIMO=max(fila),MINIMO=min(fila),MEDIA=mean(fila))
      estadisticos<-rbind(estadisticos,calculo)
    }
  }
  if (i=="S") {
    tabla_estimadores_paramericos<-data.frame(MODELO=i,estadisticos)
    grafica<-tabla_estimadores_paramericos
  }else{
    tabla_estimadores_paramericos<-data.frame(tabla_estimadores_paramericos,data.frame(MODELO=i,estadisticos))
    grafica<-rbind(grafica,data.frame(MODELO=i,estadisticos))
    }
  
}

tabla_estimadores_paramericos<-data.frame(N_observ=estimador_parametrico$N,tabla_estimadores_paramericos)
grafica<-data.frame(N_encuentros=rep(estimador_parametrico$N,time=3),grafica)

tabla_estimador_parametrico_riqueza=estimateR(colSums(formato_diversidad))
tabla_estimador_parametrico_riqueza<-cbind(ESTIMADORES=names(tabla_estimador_parametrico_riqueza),VAOLOR=tabla_estimador_parametrico_riqueza)

tabla_estimador<-list(tabla_estimador_parametrico_riqueza,tabla_estimadores_paramericos)
names(tabla_estimador)<-c("ESTIMADOR DE RIQUEZA","MODELADO DE EIQUEZA")

excel_guradado(tabla = tabla_estimador,ruta = ruta_informacion_generada,nombre = "estimadores parametrico de riqueza")


grafica<-ggplot(grafica,aes(x = N_encuentros,y = MEDIA,color = MODELO,ymax=MAXIMO,ymin=MINIMO))+
  geom_ribbon(fill=8,color=0)+
  geom_line()+
  #geom_point()+
  #geom_linerange()+
  facet_wrap(~MODELO,strip.position = "bottom")+
  ggtitle("CURVAS ESTIMADOR DE RIQUEZA PARAMETRICOS")+
  labs(x="Encuentros",y="Estimado")+
  scale_fill_calc()+
  theme(
    panel.background = element_rect(fill = "white",colour = "black"),
    panel.grid.major.y = element_line(colour = "#C2C2C2",linewidth = 0.5),
    text = element_text(family = "Times New Roman"),
    legend.position = "bottom",
    plot.title = element_text(size = 10,color = "skyblue",face = "bold",vjust = 0.5,hjust = 0.5),
    axis.title = element_text(size = 6,face = "italic"),
    axis.text = element_text(size = 5,face = "bold"),
    strip.text = element_text(colour = "white",size = 8,face = "italic",hjust = 0.5,vjust = 0.5),
    strip.background = element_rect(fill = "#597CFF",color = "black",linewidth = 0.5),
    legend.text = element_text(size = 8,face = "italic"),
    legend.title = element_text(size = 8,face = "bold")
  )
print(grafica)
exportador_graficas(grafica = grafica,ruta = ruta_graficos,nombre = "estimadores parametricos",alto_cm = 7,ancho_cm = 15)

rm(ayudar,calculo,e,estadisticos,estimador_parametrico,fila,G1,grafica,i,nombres_est,RAREFACCION_ESP,tabla_estimador,
   tabla_estimador_parametrico_riqueza,tabla_estimadores_paramericos,vector_inter)

#estimador de diversidad no parametricos raferacion------------------------------------------------------------------------------

estimador_no_parametrico<-poolaccum(formato_diversidad,permutations = 100)

estimadores<-c("S","chao","jack1","jack2","boot")

tabla_estimadores_no_parametricos<-NULL
grafica<-NULL

for (i in estimadores) {
  estimador<-estimador_no_parametrico[[i]]
  observaciones<-dim(estimador)[1]
  estadistico<-NULL
  for (e in 1:observaciones) {
    fila<-estimador[e,]
    calculo<-data.frame(MAXIMO=max(fila),MINIMO=min(fila),MEDIA=mean(fila))
    if (1==e) {
      estadistico<-data.frame(calculo)
    }else{
      estadistico<-rbind(estadistico,calculo)
    }
  }
  if (i=="S") {
    tabla_estimadores_no_parametricos<-data.frame(MODELO=i,estadistico)
    grafica<-tabla_estimadores_no_parametricos
  }else{
    tabla_estimadores_no_parametricos<-data.frame(tabla_estimadores_no_parametricos,MODELO=i,estadistico)
    grafica<-rbind(grafica,data.frame(MODELO=i,estadistico))
  }
}

tabla_estimadores_no_parametricos<-data.frame(N_observaciones=estimador_no_parametrico$N,tabla_estimadores_no_parametricos)
grafica<-data.frame(N_observaciones=rep(estimador_no_parametrico$N,time=5),grafica)

tabla_estimador_no_parametrico=specpool(formato_diversidad)

tabla_estimador<-list(tabla_estimadores_no_parametricos,tabla_estimador_no_parametrico)
names(tabla_estimador)<-c("MODELADO DE EIQUEZA","ESTIMADOR DE RIQUEZA")

excel_guradado(tabla = tabla_estimador,ruta = ruta_informacion_generada,nombre = "estimadores no parametricos de riqueza")

grafica<-ggplot(grafica,aes(x = N_observaciones,y = MEDIA,color = MODELO,ymax=MAXIMO,ymin=MINIMO))+
  geom_ribbon(fill=8,color=0)+
  geom_line()+
  #geom_point()+
  #geom_linerange()+
  facet_wrap(~MODELO,strip.position = "bottom")+
  ggtitle("CURVAS ESTIMADOR DE RIQUEZA NO PARAMETRICOS")+
  labs(x="Encuentros",y="Estimado")+
  scale_fill_calc()+
  theme(
    panel.background = element_rect(fill = "white",colour = "black"),
    panel.grid.major.y = element_line(colour = "#C2C2C2",linewidth = 0.5),
    text = element_text(family = "Times New Roman"),
    legend.position = "bottom",
    plot.title = element_text(size = 10,color = "skyblue",face = "bold",vjust = 0.5,hjust = 0.5),
    axis.title = element_text(size = 6,face = "italic"),
    axis.text = element_text(size = 5,face = "bold"),
    strip.text = element_text(colour = "white",size = 8,face = "italic",hjust = 0.5,vjust = 0.5),
    strip.background = element_rect(fill = "#597CFF",color = "black",linewidth = 0.5),
    legend.text = element_text(size = 8,face = "italic"),
    legend.title = element_text(size = 8,face = "bold")
  )
print(grafica)
exportador_graficas(grafica = grafica,ruta = ruta_graficos,nombre = "estimadores parametricos",alto_cm = 10,ancho_cm = 15)

rm(calculo,i,e,estadistico,estimador,estimador_no_parametrico,estimadores,fila,formato_diversidad,grafica,tabla_estimador,tabla_estimador_no_parametrico,
tabla_estimadores_no_parametricos,total_avistamientos)

#avistamientos mensulales------------------------------------------------------------------------------------------------------

fechas<-as.Date(df_base$FECHA,format="%d/%m/%Y")

avistamiento_fechas<-data.frame(ESPECIES=df_base$NOMBRE.CIENTIFICO,AÑO=as.numeric(as.character(fechas,format("%Y"))),MES_N=as.numeric(as.character(fechas,format("%m")))
                                ,AVISTAMIETOS=df_base$NUMERO.DE.AVISTAMIENTO)

avistamiento_fechas<-avistamiento_fechas%>%group_by(ESPECIES,MES_N)%>%summarise(AVISTAMIETOS=sum(AVISTAMIETOS))%>%
  mutate(MES=cut(MES_N,breaks =c(0:12),labels = c("ENERO","FEBRERO","MARZO","ABRIL","MAYO","JUNIO","JULIO","AGOSTO","SEPTIEMBRE","OCTUBRE","NOVIEMBRE","DICIEMBRE")))

grafica<-ggplot(avistamiento_fechas,aes(x = MES,y=ESPECIES,fill=AVISTAMIETOS))+
  geom_tile(color="white",lwd=0.5)+
  scale_fill_gradient(low ="orange",high = "green")+
  ggtitle("FRECUENCIA DE AVISTAMIENTOS POR MES Y ESPECIE")+
  guides(fill = guide_colourbar(title = "AVISTAMIENTOS"))+
  theme(
    text = element_text(family = "Times New Roman"),
    plot.title =element_text(size = 10,colour = "blue",face = "bold",hjust = 0.5,vjust = 0.5),
    panel.background = element_rect(fill = "white",color = "black",linewidth = 1),
    axis.text.x = element_text(size = 5,color = "black",face = "bold",angle = 45,hjust = 1),
    axis.text.y = element_text(size = 5,colour = "black",face= "italic"),
    axis.title = element_text(size = 8,color = "black",face = "bold"),
    legend.title = element_text(size = 7,color = "blue",face = "bold",vjust = 0.5),
    legend.text = element_text(size = 7,colour = "black",face = "italic",hjust = 0.5)
    )
print(grafica)
exportador_graficas(grafica = grafica,ruta = ruta_graficos,nombre = "frecuencia de observaciones mensuales",alto_cm = 15,ancho_cm = 15)

avistamiento_fechas<-transformacion_de_datos(filas = avistamiento_fechas$ESPECIES,columnas = avistamiento_fechas$MES,valor = avistamiento_fechas$AVISTAMIETOS)
avistamiento_fechas<-data.frame(ESPECIES=row.names(avistamiento_fechas),avistamiento_fechas)
excel_guradado(tabla = avistamiento_fechas,ruta = ruta_informacion_generada,nombre = "avistamientos por mes")

