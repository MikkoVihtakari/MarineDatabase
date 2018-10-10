#' @title Make species abbreviations from latin species names
#' @description The function generates species names from latin names
#' @param x character vector containing latin species names to be abbreviated. Taxonomic levels must be separated by whitespace. 
#' @param id logical indicating whether the abbreviation should be a computer ID (\code{TRUE}) or scientific abbreviation (\code{FALSE}) 
#' @return Returns a character vector containing species abbreviations. The abbreviations should be valid data.frame column names. Check the uniquety of the returned names (that is not checked by the function).
#' @export

make_spabbr <- function(x, id = TRUE) {
  
  sapply(x, function(j) {
    
    k <- unlist(strsplit(j, "\\s"))
    
    if(id) {
      
      if(length(k) == 1) {
        if(k[1] == "Copepoda") {
          paste0(toupper(substr(k[1], 1, 3)), "naup") 
        } else {
          substr(k[1], 1, 6)    
        }
  
        } else if(k[2] %in% c("spp.", "sp.")) {
          toupper(substr(k[1], 1, 6))
        } else if(k[1] == "Hyperia" | k[1] == "Hyperoche") {
          paste0(toupper(substr(k[1], 1, 3)), toupper(substr(k[1],(nchar(k[1])+1)-2,nchar(k[1]))), substr(k[2], 1, 3))
        } else {
          paste0(toupper(substr(k[1], 1, 3)), substr(k[2], 1, 3))  
  
    }} else {
    
      if(length(k) == 1) {
        k
      } else if(k[2] %in% c("spp.", "sp.")) {
        k
      } else {
        paste0(toupper(substr(k[1], 1, 1)), ". ", k[2]) 
      }
      
  }
    
})

}
  
  