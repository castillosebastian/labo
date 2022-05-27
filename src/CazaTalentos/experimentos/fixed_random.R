

fixed_random <-  function(tiros, vec){
  
  upper_band = round(tiros * 0.7)
  lower_band = round(tiros * 0.501)
  
  vec[vec > upper_band] <- upper_band
  vec[vec < lower_band] <- lower_band
  
  vec
  
}

new_aciertos = fixed_random(tiros_preseleccion, aciertos)