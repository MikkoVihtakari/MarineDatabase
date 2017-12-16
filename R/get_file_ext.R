#' @title Get file extension
#' @description Extracts file extension from a file name
#' @param fn File name as a character vector
#' @source The function is borrowed from StackOverflow (\url{https://stackoverflow.com/a/45467246/1082004})
#' @author Pisca46
#' @export


get_file_ext <- function (fn) {

splitted    <- strsplit(x=fn, split="/")[[1]] # remove a path. use .Platform$file.sep in stead of "/"?

fn          <- splitted [length(splitted)]
ext         <- ""
splitted    <- strsplit(x=fn, split="\\.")[[1]]
l           <-length (splitted)

if (l > 1 && sum(splitted[1:(l-1)] != "")) ext <- splitted[l] # the extention must be the suffix of a non-empty name

ext
}