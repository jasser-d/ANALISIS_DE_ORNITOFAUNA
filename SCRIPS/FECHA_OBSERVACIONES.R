#generacion de grafico en ggplo2 para meses mayor numero de meses de observacion en del numero de avistamiento 
#y numero agrupados por meses

#library(ggplot2)
#library(dplyr)
#library('ggwordcloud')


GRAFICO_DE_MESES_MAYOR_OBSERVACION<-function(ESPECIES,OBSERVACIONES,FECHA){
  
  FECHA<-as.numeric(format(FECHA,"%m"))
  
  datos1<-data.frame(ESPECIES,FECHA,OBSERVACIONES)
  
  datos1<-datos1%>%group_by(ESPECIES,FECHA)%>%summarise(OBSERVACIONES=sum(OBSERVACIONES))
  
  FECHA<-data.frame()
  
  for (i in 1:as.numeric(length(datos1$FECHA))) {
    auxiliar<-switch (as.numeric(datos1[i,2]),
                      "Enereo",
                      "Febrero",
                      "Marzo",
                      "Abril",
                      "Mayo",
                      "Junio",
                      "Julio",
                      "Agosto",
                      "Septiembre",
                      "Octubre",
                      "Noviembre",
                      "Diciembre"
    )
    FECHA<-rbind(FECHA,auxiliar)
  }  
  
  datos1<-datos1%>%select(-FECHA)%>%data.frame(FECHA=FECHA)
  
  colnames(datos1)<-c("ESPECIES","OBSERVACIONES","FECHA")
  mes_especie_avistamiento<-ggplot(datos1,aes(x=FECHA,y=ESPECIES,fill=OBSERVACIONES))+
    geom_tile(color="white",lwd=1)+
    scale_fill_gradient(low ="orange",high = "green")+
    geom_text(aes(label=OBSERVACIONES),color="black",size=2)+
    #coord_fixed()+
    ggtitle("FRECUENCIA DE AVISTAMIENTOS POR MES Y ESPECIE")+
    guides(fill = guide_colourbar(title = "AVISTAMIENTOS"))+
    theme(
      title = element_text(size = 12,colour = "blue",face = "bold"),
      axis.text.x = element_text(size = 5,color = "black",face = "bold",angle = 45),
      axis.text.y = element_text(size = 5,colour = "black",face= "italic"),
      axis.title = element_text(size = 8,color = "black",face = "bold"),
      legend.title = element_text(size = 7,color = "blue",face = "bold",vjust = 0.5),
      legend.text = element_text(size = 7,colour = "black",face = "italic")
    )
    
  datos2<-datos1%>%group_by(FECHA)%>%summarise(AVISTAMIENTOS=sum(OBSERVACIONES))
  
  mes_avistamietno<-ggplot(datos2,aes(label=FECHA,size=AVISTAMIENTOS,color=AVISTAMIENTOS))+
    geom_text_wordcloud()+
    scale_size_area(max_size = 10) +
    theme_minimal()+
    ggtitle("AVISTAMIENTOS POR MES")
    
  resultados<-list(mes_especie_avistamiento,mes_avistamietno)
  
  return(resultados)
}

