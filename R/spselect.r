spselect <- function(code, df){
  # input: species code
  # processing: select species records
  # output: selected records from table for a species as a named object, SP.[$Code]
  assign(paste("SP.", code, sep=""), df[(df$Species==paste(code)),], envir = .GlobalEnv)
  
}
