#BASE_CURVAS_DE_ESPECIES
#library(BiodiversityR)
#library(vegan)
#library("DT")


modelos_de_curvas_de_acumulacion<-function(FORMATO_DIVERSIDAD_VEGAN,PERMUTACIONES){
  
  curva_desc<-specaccum(FORMATO_DIVERSIDAD_VEGAN,method = "collector")
  
  curva_desc<-data.frame(ACUMULACION_SITIOS=as.numeric(curva_desc[["sites"]]),ACUMULACION=as.numeric(curva_desc[["richness"]]))
  
  #curva de acumulacion de especies por el metodo de randon
  
  curva_random<-specaccum(FORMATO_DIVERSIDAD_VEGAN,method = "random",permutations = PERMUTACIONES)
  
  curva_random<-data.frame(RANDON_SITIOS=curva_random[["sites"]],RANDON_ESTIMADOR=as.numeric(curva_random[["richness"]]),
                           RANDON_DESV_EST=as.numeric(curva_random[["sd"]]))
  
  #curva de acumulacion de especies por el metodo exact
  
  curva_exact<-specaccum(FORMATO_DIVERSIDAD_VEGAN,method = "exact",permutations = PERMUTACIONES)
  
  curva_exact<-data.frame(EXACT_SITIOS=curva_exact[["sites"]],EXACT_ESTIMADOR=as.numeric(curva_exact[["richness"]]),
                          EXACT_DESV_EST=as.numeric(curva_exact[["sd"]]))
  
  #curva de acumulacion de especies por el metodo coleman
  
  curva_coleman<-specaccum(FORMATO_DIVERSIDAD_VEGAN,method = "coleman",permutations = PERMUTACIONES)
  
  curva_coleman<-data.frame(COLEMAN_SITIOS=curva_coleman[["sites"]],COLEMAN_ESTIMADOR=as.numeric(curva_coleman[["richness"]]),
                            COLEMAN_DESV_EST=as.numeric(curva_coleman[["sd"]]))
  
  #curva de acumulacion de especies por el metodo rarefaccion
  
  curva_rarefaction<-specaccum(FORMATO_DIVERSIDAD_VEGAN,method = "rarefaction",permutations = PERMUTACIONES)
  
  curva_rarefaction<-data.frame(RAREFACCION_SITIOS=curva_rarefaction[["sites"]],RAREFACCION_ESTIMADOR=as.numeric(curva_rarefaction[["richness"]]),
                                RAREFACCION_DESV_EST=as.numeric(curva_rarefaction[["sd"]]))
  
  #CREAR BASE DE DATOS
  
  DATOS_MODELO<-data.frame(cbind(curva_desc,curva_random,curva_exact,curva_coleman,curva_rarefaction))
  
  return(DATOS_MODELO)
}
