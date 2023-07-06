
#calculo de diversidad 

DIVERSIDAD_ALFA<-function(base_datos){
 
  #DIVERSIDAD TOTAL DEL INVENTARIO DE ORNITOFAUNA
  
  abund<-data.frame(colSums(base_datos))
  
  SIMPSOM<-diversity(abund,index = "simpson")
  SHANNOM<-diversity(abund,index = "shannon")
  
  MENHINICK<-length(abund)/sqrt(sum(abund))
  
  TOTAL<-data.frame(SIMPSOM,SHANNOM,MENHINICK)
  
  #DIVERSIDADE PARA CADA PUNTO DE OBSERVACION
  SIMPSOM<-diversity(base_datos,index = "simpson")
  SHANNOM<-diversity(base_datos,index = "shannon")
  
  ciclo<-1:dim(base_datos)[1]
  MENHINICK<-data.frame()
  
  for (i in ciclo) {
    filas<-formato_vegan_parcelas[i,]
    filas<-filas[0!=filas]
    total<-length(filas)/sqrt(sum(filas))  
    if (i==1) {
      MENHINICK<-cbind(total)
    }
    else{
      MENHINICK<-cbind(MENHINICK,total)
    }
  }
  
  colnames(MENHINICK)<-c(row.names(base_datos))
  
  PARCELAS<-rbind(PARCELAS=row.names(base_datos),SIMPSOM,SHANNOM,MENHINICK)%>%t()%>%data.frame()
  colnames(PARCELAS)<-c("PARCELAS","SIMPSOM","SHANNOM","MENHINICK")
  indices<-list(TOTAL,PARCELAS)
  names(indices)<-c("TOTAL","PARCELAS")
  return(indices)
}
