#' @title Generate a species list containing unique levels from a taxonomy dataset
#' @description Generate a data frame containing unique species names, summed abundances and search terms to be used in XXX
#' @param dat data frame containing taxonomy information. Must be in long format.
#' @param sp.col Character specifying the column name that contains species names in \code{dat}.
#' @param ab.col Character specifying the column name that contains species abundances in \code{dat}.
#' @param st.col Character specifying the column name that contains station names in \code{dat}.
#' @param expedition Character specifying the column name that contains expedition in \code{dat}. Not required. Defaults to \code{NA}.
#' @return Returns a list containing a data frame (\code{$species_list}) the original species names (as unique levels in \code{dat}; or_name), summed abundance (ab), number of stations where the species name was encountered (n), total number of station (N), a tidied search term for XXX function (search_term), type of species (spores and epiphytes are separated, otherwise \code{NA}; type) and certainty of species identification (includes sp., spp., aff. and cf.; certainty)
#' @author Mikko Vihtakari
#' @export

#dat = dat; sp.col = "species"; ab.col = "abundance"; st.col = "station"; expedition = "ICE2011"; additional = NULL

make_species_list <- function(dat, sp.col = "species", ab.col = "abundance", st.col = "station", expedition = NA) {

  ## Section start ####
  if(!is.factor(dat[[st.col]])) dat[[st.col]] <- factor(dat[[st.col]])
  dat[[sp.col]] <- factor(dat[[sp.col]])
  
  ## Summarized information
  
  sum.info <- data.frame(expedition = expedition, N = nlevels(dat[[st.col]]), abSum = sum(dat[[ab.col]], na.rm = TRUE))
  
  ## Original unique names, abundances and frequency of occurrence
  
  tmp <- lapply(split(dat, dat[[sp.col]]), function(k) {
    
    tp <- k[!is.na(k[[ab.col]]),]
    tp <- tp[tp[[ab.col]] > 0,]
  
    out <- data.frame(or_name = as.character(unique(k[[sp.col]])), ab = sum(k[[ab.col]], na.rm = TRUE), n = length(as.character(unique(tp[[st.col]]))))
    out$perAb <- 100*out$ab/sum.info$abSum
    out$fo <- 100*out$n/sum.info$N
    out
  })
  
  spls <- do.call(rbind, tmp)
  
  spls <- spls[order(spls$or_name),]
  row.names(spls) <- 1:nrow(spls)

  ## Add search term and modify it ###

  spls <- rapply(object = spls, f = as.character, classes = "factor", how = "replace")

  spls$search_term <- spls$or_name

  # add spaces after .
  spls$search_term <- gsub("(\\.)(\\S)", "\\1 \\2", spls$search_term, perl = TRUE) 
  
  # add cysts as type
  spls$type <- ifelse(grepl("(spores\\b|spore\\b|cyst\\b|cysts\\b|cysta\\b|spora\\b)", spls$search_term, ignore.case = TRUE, perl = TRUE), "spore", 
    ifelse(grepl("(pollen)", spls$search_term, perl = TRUE), "pollen",
    ifelse(grepl("(symbiont)", spls$search_term, perl = TRUE, ignore.case = TRUE), "symbiont",
    ifelse(grepl("(epiphyte)", spls$search_term, perl = TRUE), "epiphyte", NA))) )

  # add sp., spp., aff. and cf. to their own column
  spls$certainty <- ifelse(grepl("aff\\s", spls$search_term), "aff.",
    ifelse(grepl("\\scf", spls$search_term), "cf.",
    ifelse(grepl("(\\sindet)|(\\sident)", spls$search_term), "indet.",
    ifelse(grepl("sp\\.", spls$search_term), "sp.",
    ifelse(grepl("unknown", spls$search_term, ignore.case = TRUE), "unknown", NA)))))

  # remove aff and cf.
  spls$search_term <- gsub("(^aff\\s)|(\\sindet.)|(\\sident.)|(\\ssp.\\scf.\\s\\w+\\S+)|(\\ssp.\\scf\\s\\w+\\S+)|(\\scf.\\s\\w.\\s)|(\\scf\\s)|(\\scf[\\.]\\s)", 
    " ", spls$search_term, perl = TRUE) 

  # remove sizes
  spls$search_term <- gsub("\\s\\S+\\d\\w[m]$|\\s\\S+\\d+\\s\\S[m]$", "", spls$search_term, perl = TRUE)
  
  # remove everything inside brackets
  spls$search_term <- gsub("\\([^\\)]*\\)", "", spls$search_term, perl = TRUE) 

  # remove all cysts, spores, etc.
  spls$search_term <- gsub("\\scyst\\b|\\s\\b\\w+cyst\\b|\\scysts\\b|\\s\\b\\w+cysts\\b|\\sspore\\b|\\sspores\\b|\\s\\b\\w+spore\\b|\\s\\b\\w+spores\\b|\\spollen\\b|\\bcysta\\b|\\bspora\\b|\\bsymbiont\\b", 
    "", spls$search_term, perl = TRUE, ignore.case = TRUE) 

  # remove numbers
  spls$search_term <- gsub("(\\s\\d+)|(\\d+)", "", spls$search_term, perl = TRUE) 

  # add . after sp
  spls$search_term <- gsub("(\\ssp\\s)|(\\ssp$)", " spp.", spls$search_term, perl = TRUE) 

  # replace sp. with spp.
  spls$search_term <- gsub("(\\ssp.\\s)|(\\ssp.$)", " spp.", spls$search_term, perl = TRUE) 
  
    # Remove descriptive words
  spls$search_term <- gsub("(large\\s)|(round\\s)|(naked\\s)|(tiny\\s)|(wide\\s)|(thin\\s)|(oval\\s)|(shaped\\s)|(thecate\\s)|(bean-shaped\\s)|(-like$)|(\\sin\\sribbon\\b)", "", spls$search_term, perl = TRUE, ignore.case = TRUE)
  
  # Replace common names etc.
  spls$search_term <- gsub("(dino\\s)|(dinos\\s)|(dino$)|(dinos$)", "Dinoflagellata", spls$search_term, perl = TRUE, ignore.case = TRUE)
  spls$search_term <- gsub("(dinoflagellate\\s)|(dinoflagellates\\s)|(dinoflagellate$)|(dinoflagellates$)", "Dinoflagellata", spls$search_term, perl = TRUE, ignore.case = TRUE)
  spls$search_term <- gsub("(pennate\\sdiatom\\b)|(pennate\\sdiatoms\\b)", "Pennales", spls$search_term, perl = TRUE, ignore.case = TRUE)
  spls$search_term <- gsub("(centric\\sdiatom\\b)|(centric\\sdiatoms\\b)|(centriceae\\b)", "Coscinodiscophyceae", spls$search_term, perl = TRUE, ignore.case = TRUE)
  spls$search_term <- gsub("(choreotrich\\sciliate\\b)|(choreotrich\\sciliates\\b)", "Choreotrichia", spls$search_term, perl = TRUE, ignore.case = TRUE)
  spls$search_term <- gsub("(cryptophyte\\b)|(cryptophytes\\b)", "Cryptophyceae", spls$search_term, perl = TRUE, ignore.case = TRUE)
  spls$search_term <- gsub("(ciliate\\b)|(ciliates\\b)", "Ciliophora", spls$search_term, perl = TRUE, ignore.case = TRUE)
  spls$search_term <- gsub("(flagellates\\b)|(flagellates$)", "flagellate", spls$search_term, perl = TRUE, ignore.case = TRUE)
  spls$search_term <- gsub("(choanoflagellate\\b)", "Choanoflagellatea", spls$search_term, perl = TRUE, ignore.case = TRUE)
  spls$search_term <- gsub("(coccolithophores)|(coccolithophore)", "Prymnesiophyceae", spls$search_term, perl = TRUE, ignore.case = TRUE)
  spls$search_term <- gsub("(naviculoid)", "Navicula", spls$search_term, perl = TRUE, ignore.case = TRUE)
  spls$search_term <- gsub("(scuticociliophora)", "Scuticociliatia", spls$search_term, perl = TRUE, ignore.case = TRUE)
  spls$search_term <- gsub("(dinophycean)", "Dinophyceae", spls$search_term, perl = TRUE, ignore.case = TRUE)
  
    ## Final tuning ####
  # Remove spp., # this if needed
  spls$search_term <- gsub("spp.", "", spls$search_term, perl = TRUE) 
  
  # remove double+ spaces
  spls$search_term <- gsub("\\s{2,}", " ", spls$search_term, perl = TRUE) 
  
  # Replace empty search terms by NA
  spls$search_term[spls$search_term == ""] <- NA
  
  # remove all double names for now
  spls$search_term <- gsub("\\/.+", "", spls$search_term, perl = TRUE) 

  # remove extra white space
  spls$search_term <- trimws(spls$search_term) 

  # Remove search terms that contain only one character
  spls$search_term[nchar(spls$search_term) <= 2] <- NA
  
  # Replace unknowns by NA
  spls$certainty[tolower(spls$search_term) %in% c("unknown", "incertain taxa", "incertains taxa", "unknown taxon")] <- "unknown"
  
  spls$search_term[tolower(spls$search_term) %in% c("unknown", "incertain taxa", "incertains taxa", "unknown taxon")] <- NA
  
  # remove extra white space at the end of some words (unknown format, trimws does not always work)
  spls$search_term <- gsub("[[:space:]]$", "", spls$search_term)
  
  # only first word as capital letter
  spls$search_term <- gsub("(^\\w)", "\\U\\1", tolower(spls$search_term), perl=TRUE)

  ## Species list column classes
  
  spls <- rapply(spls, factor, classes = "character", how = "replace")
  
  ## return the species list ####
  out <- NULL
  out$data <- spls
  out$sum_info <- sum.info
  class(out) <- "speciesList"
  out
}
