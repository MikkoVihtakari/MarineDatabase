#' @title Guess gear types from a list of candidates
#' @description Uses fuzzy matching (\code{\link[base]{agrep}}) to guess gear types from a list of allowed character strings (see \code{\link{GEAR}} and \code{\link{TYPES}}).
#' @param x a character vector containing initial gear types to be changed. 
#' @param type a character vector containing sample types for \code{x}. Must be as long as \code{x}
#' @param change_valid Logical indicating whether the function should try to change already valid gear types according to the gear list (see \code{\link{GEAR}}). Defaults to \code{FALSE}. This option might be useful, if you suspect that there are mistakes in the listed gear types.
#' @param force Logical indicating whether gear types should be forced to allowed gear types as listed in \code{\link{TYPES}}. 
#' @return Returns a character vector containing matched gear types. Returns an error if a gear type was not found.


x = c("Ice corer 14cm"); type = "IAT"; change_valid = FALSE; force = FALSE
k <- x[1]
tp <- type[1]

x <- as.character(x)
type <- as.character(type)

mapply(.guess_gear, k = x, tp = type)

.guess_gear <- function(k, tp, Change_valid = change_valid, Force = force) {
  if(Change_valid) {
    list(gear = as.character(k), type = as.character(tp))
  } else {
    "bla"
  }
}

  
  # Load allowed type data frames

  
})

if(Change_valid & k %in% GEAR$gear) {
  out <- k
} else {
  
}

data.frame(org = gear_test, mod = sub("(\\d+)(\\B\\D[m])$", "\\1 \\2", gear_test, perl = TRUE)) # \B([µμc][m])$
 
  if(tp == "CTD") {
    out <- TYPES[TYPES$code == "CTD", "gear_type"]
  } else {
    if(tp == "CTM") {
      out <- TYPES[TYPES$code == "CTM", "gear_type"]
    } else {
    
      if(!as.character(k) %in% GEAR$gear) {
        
        out <- agrep(k, GEAR$gear, value = TRUE)

      # Exceptions
          if(k == "Multinet") out <- grep("200", temp_gear, value = TRUE)
          if(length(out) != 1) out <- agrep(gsub("[[:punct:]]", " ", tmp$gear), GEAR$gear, value = TRUE)
          if(length(out) != 1 & length(TYPES[TYPES$code == tp, "gear_type"]) != 0) temp_gear <- TYPES[TYPES$code == tmp$temp_type, "gear_type"]
          if(length(temp_gear) != 1) stop(paste("Fuzzy matching", tmp$gear, "does not work for row", i))    
  } else {
    temp_gear <- tmp$gear
  }}}
  
    tmp$gear <- temp_gear
    tmp
})

