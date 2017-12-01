#' @title Convert sampling depths to numeric.
#' @description  Converts sampling depths to numeric and adds any character strings from these columns to comments
#' @param dt data frame containing meta-data. See \code{\link{export_metadata}}
#' @param column column name in meta-data containing the sampling depths to be converted.
#' @param comment.col the name of the comment column in \code{dt}
#' @author Mikko Vihtakari
#' @export

#column = "from"
#comment.col = "comment"

convert_sampling_depths <- function(dt, column, comment.col = "comment") {
  dt[,column] <- as.character(dt[,column])

  tp <- lapply(1:nrow(dt), function(i) { ## Add any milliQs etc to comments
  #print(i)
  tmp <- dt[i,]

tmp$comment <- ifelse(grepl("[[:digit:]]", tmp[,column]), tmp[,comment.col], if(is.na(tmp[,comment.col])) {
    tmp[,column]
  } else {
      paste(tmp[,comment.col], tmp[,column], sep = ".")
  }
)

tmp
})

dt <- do.call(rbind, tp)

suppressWarnings(dt[,column]  <- as.numeric(dt[,column]))
message(paste0("'", column, "'", " column converted to numeric. NAs produced. Any character strings in the column have been moved to the 'comment' column."))

dt
}
