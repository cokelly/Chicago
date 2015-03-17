# Formula to isolate cases by copyright sign, and to ditch the metadata at the start, then create a dataframe with case name, code, date and body

westlaw_cases <- function(x, key){
# The copyright notice below acts as the separator between cases in a Westlaw cases txt file
      
separator <- which(x == "© 2015 Sweet & Maxwell")

# isolate odd separator markers (beginning of cases)
o0 <- seq(1, length(separator), by=2)
sep_odd <- separator[o0]

# isolate even separator markers (end of cases)
e0 <- seq(2, length(separator), by=2)
sep_even <- separator[e0]

#create an empty container matrix

full_cases.m <- matrix(nrow = length(sep_begin), ncol = 5)

# work through the dataset using the information isolated above in order to isolate on a case-by-case basis
for(i in 1:length(sep_begin)){
      
      # Start by isolating the case name
      # This is complicated by the fact that the case name is placed inconsistently in different places across cases. So I work through a series of assumptions
      # 1. That there is a ' v ' in the line (as in "Eden v Foster")
      # 2. If there is nothing with that character (as in "re Duomatic"), then take the first line
      # 3. If there are more than one lines with ' v ' in the line, then take the first one.
      
      a <- x[(sep_begin[i]+1):(sep_end[i]-1)]
      top <- a[1:8]
      # assume there's a "v" in the name
      name0 <- grep(" v ", top)
      if(length(name0) == 1){
      full_cases.m[i,1] <- top[name0]
      } else {
      # if not, take the first line
            if(length(name0) == 0){
                  full_cases.m[i,1] <- top[1]
      # if there are more than one lines with "v" in the first ten lines, choose the first one
            } else {
                  if(length(name0) > 1){
                        full_cases.m[i,1] <- name0[1]
                  }
            }
            }
      
      
      # Now the dates, court and code
      # 1. Find a line with a month name in the first 12 lines
      # 2 Take the following line as the case code (that's where they usually are)
      # 3. Take the line before as the court (Chancery etc)
      # If there is nothing there (the date line was at the top), then return an NA
      
months <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
      
for(q in 1:12){
     m <- grep(months[q], top)
     if(length(m) > 0){
           #code <- m-1
           #import dates
           full_cases.m[i,2] <- top[m[1]]
           #import codes
           full_cases.m[i,3] <- top[(m[1])+1]
           #import court
           if(length(top[(m[1])-1]) > 0){
           full_cases.m[i,4] <- top[(m[1])-1]
           } else {
                full_cases.m[i,4] = NA
           }
     }
}
      # I'm going to have to change this to get 50 words on each side of the key word.
isolatekey <- function(a, key){

      collapsed0 <- strsplit(a, " ")
      collapsed <- unlist(collapsed0)
      
      #Isolated the keyword and fullstops
      
      unacc <- grep("unaccountable", collapsed)
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

sentence <- isolatekey(a, key)
      full_cases.m[i,5] <- sentence

}

y <- as.data.frame(full_cases.m)

# Proper column names
colnames(y) <- c("Case", "Date", "Code", "Court", "Body")

#Format date
y$Date <- as.Date(y$Date, format="%d %B %Y")

#Remove leading page numbers
y[,1] <- gsub('\\*[[:digit:]]+', '', y[,1])

return(y)}
