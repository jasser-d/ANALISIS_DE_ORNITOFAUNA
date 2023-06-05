
#calculo de diversidad 

#library(asbio)


DIVERSIDAD_ALFA<-function(base_datos){
 
  #DIVERSIDAD TOTAL DEL INVENTARIO DE ORNITOFAUNA
  
  abund<-data.frame(rowSums(base_datos))
  
  SIMPSOM<-alpha.div(abund,"simp")
  SHANNOM<-alpha.div(abund,"shan")
  
  TOTAL<-data.frame(SIMPSOM,SHANNOM)
  
  #DIVERSIDADE PARA CADA PUNTO DE OBSERVACION
  
  PARCELAS<-rbind(alpha.div(base_datos,"shan"),alpha.div(base_datos,"simp"))
  rownames(PARCELAS)<-c("DIVERSIDAD DE SHANNON","DIVERSIDAD DE SIMPSON")
  PARCELAS<-t(PARCELAS)
  
  PARCELAS<-data.frame('PUNTO DE OBSERVACION'=row.names(PARCELAS),PARCELAS)
  
  indices<-list(TOTAL,PARCELAS)
  
  return(indices)
}
