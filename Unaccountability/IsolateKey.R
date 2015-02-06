isolatekey <- function(a, key){
      
      collapsed0 <- strsplit(a, " ")
      collapsed <- unlist(collapsed0)
      
      #Isolated the keyword and fullstops
      
      unacc <- grep(key, collapsed)
      fullstops <- grep("\\.", collapsed)
      
      # Now isolate the relevant sentence
      if(length(unacc) == 1){
            end00 <- which(fullstops >= unacc)
            end0 <- fullstops[end00]
            end <- end0[1]
            
            start00 <- which(fullstops < unacc)
            start0 <- fullstops[start00]
            start <- start0[length(start0)]+1
            
            sentence <- collapsed[start:end]
            
            sentence <- paste(sentence, collapse = " ")
      } else {
            if(length(unacc) > 1)
            {
                  d = NULL
                  for(i in 1:length(unacc)){
                        end00 <- which(fullstops >= unacc[i])
                        end0 <- fullstops[end00]
                        end <- end0[1]
                        
                        start00 <- which(fullstops < unacc[i])
                        start0 <- fullstops[start00]
                        start <- start0[length(start0)]+1
                        
                        sentence0 <- collapsed[start:end]
                        
                        sentence0 <- paste(sentence0, collapse = " ")     
                        sentence <- c(d, sentence0)
                        
                  }
            }
      }
      return(sentence)   
      
}
