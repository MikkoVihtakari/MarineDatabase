#' @title Guess column names from a list of candidates
#' @description Uses fuzzy matching (\code{\link[base]{agrep}}) to guess (column) names from a list of allowed character strings.
#' @param cols Character vector of approximate column names to be guessed
#' @param df data frame containing column names
#' @param candidates a switch argument or a character vector giving the candidates to be used in matching. The \code{\link{coln_search_words}} function is used by default.
#' @author Mikko Vihtakari, Conrad Helgeland
#' @export

# cols = required_cols; df = dt; candidates = coln_search_words

guess_colname <- function(cols = required_cols, df = dt, candidates = coln_search_words) {
  
  sapply(cols, function(k) {
    
    if(any(k %in% colnames(df))) {
      candidates(k, return_name = TRUE)
    } else {
      colnames(df)[grep(candidates(k), gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", gsub("[[:punct:]]", " ", colnames(df)), perl = TRUE), ignore.case = TRUE, perl = TRUE)][1]
    }
    
  })
}