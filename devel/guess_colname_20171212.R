#' @title Guess column names from a list of candidates
#' @description Uses fuzzy matching (\code{\link[base]{agrep}}) to guess (column) names from a list of allowed character strings.
#' @param cols Character vector of correct column names to be found
#' @param df Character vector of wrong column names to be returned
#' @param candidates a switch argument or a character vector giving the candidates to be used in matching
#' @author Mikko Vihtakari
#' @export

# cols = levels(dt$variable); df = df; candidates = 

guess_colname <- function(cols = required_cols, df = dt, candidates = coln_search_word) {
  
  
  
  
  
  sapply(cols, function(k) {
    colnames(df)[agrep(candidates(k), gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", gsub("[[:punct:]]", " ", colnames(df)), perl = TRUE), ignore.case = TRUE)][1]
  })
}