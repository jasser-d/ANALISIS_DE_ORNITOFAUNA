#datos para diversidad alfa

transformacion_de_datos<-function(filas,columnas,valor){
  
  #agrupar datos y renombrar variables
  
  datos<-data.frame(filas,columnas,valor=as.numeric(valor))
  
  datos<-datos%>%group_by(filas,columnas)
  
  # suma de las observaciones en funcion de los grupos que se repite las observaciones
  
  datos<- summarise(datos,valor=sum(valor))
  
  #extraer nombres de las especies encontradas, numero de observaciones y puntos de observacion (nombre de 
  #de parcelas, trasnceptos o puntos de observacion)
  
  var_columnas<-unique(datos$columnas)
  
  var_filas<-unique(datos$filas)
  
  largo<-length(var_columnas)
  
  resultados<-data.frame(var_filas)
  
  colnames(resultados)<-c("ELIMINAR")
  
  #clasificar y agrupar datos
  
  for (i in 1:largo) {
    filtrado<-(datos%>%filter(columnas==var_columnas[i]))
    calculado<-group_by(filtrado,filas)%>%summarise(valor=sum(valor))
    
    colnames(calculado)<-c("V",i)
    
    resultados<-merge(x=resultados,y=calculado,all.x = T,by.x =c("ELIMINAR"),by.y = c("V"))
  }
  
  #nombre de las variabkles y columnas, ademas de eliminar una variable de apoyo
  
  rownames(resultados)<-resultados$ELIMINAR
  
  resultados<-resultados%>%select(-ELIMINAR)
  
  colnames(resultados)<-var_columnas
  
  #eliminar datos nulo y retorno de la tabla para el uso de la libreria "VEGAN"
  
  resultados[is.na(resultados)] <- 0
  
  return(resultados)
}
