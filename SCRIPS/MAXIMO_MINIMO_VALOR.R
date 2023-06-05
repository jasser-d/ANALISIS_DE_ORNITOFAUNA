#obtiene los valores maximos y minimos de las filas 




MAX_Y_MIN<-function(Matriz_base){
  
  Matriz_base<-data.frame(Matriz_base)
  
  filas_n<-as.numeric(count(Matriz_base))
  
  resultado<-data.frame()
  
  for (i in 1:filas_n) {
    maximo<-Matriz_base[i,]%>%max()
    minimo<-Matriz_base[i,]%>%min()
    auxiliar<-cbind(maximo,minimo)
    resultado<-rbind(resultado,auxiliar)
  }
  colnames(resultado)<-c("MAXIMO","MINIMO")
  return(resultado)
}