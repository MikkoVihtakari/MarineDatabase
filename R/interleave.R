#' @title Interleave vectors into a single vector
#' @description Interleave vectors into a single vector
#' @param ... vectors to interleave
#' @export

interleave <- function(...) {
  c(rbind(...))
}
