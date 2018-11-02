#' @title Convert font sizes measured as points to ggplot font sizes
#' @description Converts font sizes measured as points (as given by most programs such as MS Word etc.) to ggplot font sizes
#' @param x numeric vector giving the font sizes in points
#' @return Returns a numeric vector of lenght \code{x} of ggplot font sizes
#' @keywords internal
#' @export
FS <- function(x) x/2.845276 # x is the desired font / line size in pt

#' @title Convert line sizes measured as points to ggplot line sizes
#' @description Converts line sizes measured as points (as given by most programs such as Adobe Illustrator etc.) to ggplot font sizes
#' @param x numeric vector giving the lines sizes in points
#' @return Returns a numeric vector of lenght \code{x} of ggplot line sizes
#' @keywords internal
#' @export
LS <- function(x) x/2.13

#' @title Interleave vectors into a single vector
#' @description Interleave vectors into a single vector
#' @param ... vectors to interleave
#' @export
interleave <- function(...) {
  c(rbind(...))
}

#' @title Return character vector elements or length depending on length
#' @description This internal function returns character vector elements or length as a character depending on the length of \code{x}. Useful in \link[base]{stop} and \link[base]{warning} functions.
#' @param x character vector
#' @param len integer specifying the cut value. If length of \code{x} <= \code{len}, a character vector will be returned. Otherwise a length as character.
#' @export
warn_vec <- function(x, len = 5) {
  if(length(x) <= len) {
    paste(x, collapse=", ")
  } else {
    as.character(length(x))
  }
}
