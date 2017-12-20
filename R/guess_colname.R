#' @title Guess column names from a list of candidates
#' @description Uses fuzzy matching (\code{\link[base]{agrep}}) to guess (column) names from a list of allowed character strings.
#' @param cols Character vector of approximate column names to be guessed
#' @param df data frame containing column names
#' @param candidates a switch argument or a character vector giving the candidates to be used in matching
#' @author Mikko Vihtakari
#' @export

# cols = sample_name; df = df; candidates = 

guess_colname <- function(cols = required_cols, df = dt, candidates = coln_search_word) {
  
  sapply(cols, function(k) {
    colnames(df)[grep(candidates(k), gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", gsub("[[:punct:]]", " ", colnames(df)), perl = TRUE), ignore.case = TRUE, perl = TRUE)][1]
  })
}