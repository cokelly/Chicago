#This function imports all text files from a working directory

txt_files <- list.files(pattern="*.txt")

x <- length(txt_files)

files2 <- file.rename(list.files(pattern="*.txt"), paste0("file_", 1:x, ".txt"))

txt_files <- list.files(pattern="*.txt")

for(i in txt_files) {
      x <- scan(i, what="character", sep="\n")
      assign(i,x)  
}
