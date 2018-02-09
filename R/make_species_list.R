#' @title Generate a species list containing unique levels from a taxonomy dataset
#' @description Generate a data frame containing unique species names, summed abundances and search terms to be used by \code{\link{confirm_species_list}}.
#' @param dat Data frame containing taxonomy information. Must be in long format.
#' @param sp.col Character specifying the column name that contains species names in \code{dat}.
#' @param ab.col Character specifying the column name that contains species abundances in \code{dat}.
#' @param st.col Character specifying the column name that contains station names in \code{dat}.
#' @param exp.col Character specifying the column name that contains expedition in \code{dat}. 
#' @return Returns \code{speciesList} object containing a list of unique species entries in \code{dat} (\code{$data}) and summary information (\code{$sum_info}). \code{$data} contains following columns:
#' \itemize{
#'  \item \strong{\code{or_name}} The original species names (as unique levels in \code{dat}).
#'  \item \strong{\code{expedition}} Expeditions during which the species occurred. Several expeditions are separated by \code{;}.
#'  \item \strong{\code{ab}} Summed abundance for a species entry.
#'  \item \strong{\code{n}} Number of stations where the species name was encountered (n)
#'  \item \strong{\code{perAb}} Summed abundance for a species entry as a percentage of total summed abundance (\code{$sum_info$abSum}).
#'  \item \strong{\code{fo}} Frequency of occurrence for a species entry (total number of stations is given in \code{$sum_info$N}).
#'  \item \strong{\code{search_term}} A tidied search term for \code{\link{confirm_species_list}} function.
#'  \item \strong{\code{type}} Type of species (spores, symbionts and epiphytes are separated, otherwise \code{NA}).
#'  \item \strong{\code{certainty}} Certainty of species identification (includes sp., spp., aff., cf. and unknown).
#'  }
#' @author Mikko Vihtakari
#' @seealso \code{\link{confirm_species_list}} for confirming the species names against a database.
#' @export

#dat = dat; sp.col = "species"; ab.col = "abundance"; st.col = "station"; exp.col = "expedition"
#spls <- data.frame(search_term = "Centrophyceae", stringsAsFactors = FALSE)

make_species_list <- function(dat, sp.col = "species", ab.col = "abundance", st.col = "station", exp.col = "expedition") {

  ## Section start ####
  if(!is.factor(dat[[st.col]])) dat[[st.col]] <- factor(dat[[st.col]])
  dat[[sp.col]] <- factor(dat[[sp.col]])
  
  ## Summarized information
  
  sum.info <- data.frame(N = nlevels(dat[[st.col]]), abSum = sum(dat[[ab.col]], na.rm = TRUE))
  
  ## Original unique names, abundances and frequency of occurrence
  
  tmp <- lapply(split(dat, dat[[sp.col]]), function(k) {
    
    tp <- k[!is.na(k[[ab.col]]),]
    tp <- tp[tp[[ab.col]] > 0,]
  
    out <- data.frame(or_name = as.character(unique(k[[sp.col]])), expedition = paste(unique(k[[exp.col]]), collapse = "; "), 
                      ab = sum(k[[ab.col]], na.rm = TRUE), n = length(as.character(unique(tp[[st.col]]))))
    out$perAb <- 100*out$ab/sum.info$abSum
    out$fo <- 100*out$n/sum.info$N
    out
  })
  
  spls <- do.call(rbind, tmp)

  spls <- rapply(object = spls, f = as.character, classes = "factor", how = "replace")
    
  spls <- spls[order(spls$or_name),]
  row.names(spls) <- 1:nrow(spls)

  ## Add search term and modify it ###

  spls$search_term <- spls$or_name

  # add spaces after .
  spls$search_term <- gsub("(\\.)(\\S)", "\\1 \\2", spls$search_term, perl = TRUE) 
  
  # add cysts as type
  spls$type <- ifelse(grepl("(spores\\b|spore\\b|cyst\\b|cysts\\b|cysta\\b|spora\\b)", spls$search_term, ignore.case = TRUE, perl = TRUE), "spore", 
    ifelse(grepl("(pollen)", spls$search_term, perl = TRUE), "pollen",
    ifelse(grepl("(symbiont)", spls$search_term, perl = TRUE, ignore.case = TRUE), "symbiont",
    ifelse(grepl("(epiphyte)", spls$search_term, perl = TRUE), "epiphyte", NA))) )

  # add sp., spp., aff. and cf. to their own column
  spls$certainty <- ifelse(grepl("aff\\s", spls$search_term, perl = TRUE), "aff.",
    ifelse(grepl("\\scf", spls$search_term, perl = TRUE), "cf.",
    ifelse(grepl("(\\sindet)|(\\sident)", spls$search_term, perl = TRUE), "indet.",
    ifelse(grepl("(\\binc.\\ssed.$)|(\\bincertae\\ssedis$)", spls$search_term, ignore.case = TRUE, perl = TRUE), "inc. sed.",
    ifelse(grepl("(\\sspp.\\s)|(\\ssp.\\s)|(\\sspp.$)|(\\ssp.$)", spls$search_term, perl = TRUE), "sp.",
    ifelse(grepl("unknown", spls$search_term, ignore.case = TRUE), "unknown", NA))))))
  
  # Remove pollen (from trees)
  spls$search_term <- ifelse(spls$type %in% "pollen", NA, spls$search_term)
  
  # remove aff and cf.
  spls$search_term <- gsub("(^aff\\s)|(\\sindet.)|(\\sident.)|(\\bindet\\b)|(\\ssp.\\scf.\\s\\w+\\S+)|(\\ssp.\\scf\\s\\w+\\S+)|(\\scf.\\s\\w.\\s)|(\\scf\\s)|(\\scf[\\.]\\s)|(\\binc.\\ssed.$)|(\\bincertae\\ssedis$)", 
    " ", spls$search_term, perl = TRUE) 

  # remove sizes
  spls$search_term <- gsub("\\s\\S+\\d\\w[m]$|\\s\\S+\\d+\\s\\S[m]$", "", spls$search_term, perl = TRUE)
  
  # remove everything inside brackets
  spls$search_term <- gsub("\\([^\\)]*\\)", "", spls$search_term, perl = TRUE) 

  # remove all cysts, spores, etc.
  spls$search_term <- gsub("\\scyst\\b|\\s\\b\\w+cyst\\b|\\scysts\\b|\\s\\b\\w+cysts\\b|\\sspore\\b|\\bspores\\b|\\s\\b\\w+spore\\b|\\s\\b\\w+spores\\b|\\spollen\\b|\\bcysta\\b|\\bspora\\b|\\bsymbiont\\b|\\bendosymbiont\\w\\b", 
    "", spls$search_term, perl = TRUE, ignore.case = TRUE) 

  # remove numbers
  spls$search_term <- gsub("(\\s\\d+)|(\\d+)", "", spls$search_term, perl = TRUE) 

  # remove + and & signs
  spls$search_term <- gsub("([+])|([&])", "", spls$search_term, perl = TRUE)
  spls$search_term <- gsub("(.-$)", ".", spls$search_term, perl = TRUE)
  
  # add . after sp
  spls$search_term <- gsub("(\\ssp\\s)|(\\ssp$)", " spp.", spls$search_term, perl = TRUE) 

  # replace sp. with spp.
  spls$search_term <- gsub("(\\ssp.\\b)|(\\ssp.\\s)|(\\ssp.$)", " spp.", spls$search_term, perl = TRUE) 
  
  # Remove descriptive words
  spls$search_term <- gsub("(large\\s)|(round\\s)|(naked\\s)|(tiny\\s)|(small\\b)|(wide\\s)|(thin\\s)|(oval\\s)|(shaped\\s)|(thecate\\s)|(bean-shaped\\s)|(-like$)|(\\sin\\sribbon\\b)|(deformed\\s)|(\\bwith\\b)|(\\blobes\\b)|(\\bempty\\b)|(\\bheterotrophic\\b)|(elongate\\b)", "", spls$search_term, perl = TRUE, ignore.case = TRUE)
  
  # Replace common names etc.
  spls$search_term <- gsub("(dino\\s)|(dinos\\s)|(dino$)|(dinos$)", "Dinoflagellata", spls$search_term, perl = TRUE, ignore.case = TRUE)
  spls$search_term <- gsub("(dinoflagellate\\s)|(dinoflagellates\\s)|(dinoflagellate$)|(dinoflagellates$)", "Dinoflagellata", spls$search_term, perl = TRUE, ignore.case = TRUE)
  spls$search_term <- gsub("(pennate\\sdiatom\\b)|(pennate\\sdiatoms\\b)|(pennatophyceae\\b)", "Pennales", spls$search_term, perl = TRUE, ignore.case = TRUE)
  spls$search_term <- gsub("(centric\\sdiatom\\b)|(centric\\sdiatoms\\b)|(centriceae\\b)|(centrophyceae\\b)", "Centrales", spls$search_term, perl = TRUE, ignore.case = TRUE)
  spls$search_term <- gsub("(choreotrich\\sciliate\\b)|(choreotrich\\sciliates\\b)", "Choreotrichia", spls$search_term, perl = TRUE, ignore.case = TRUE)
  spls$search_term <- gsub("(cryptophyte\\b)|(cryptophytes\\b)", "Cryptophyceae", spls$search_term, perl = TRUE, ignore.case = TRUE)
  spls$search_term <- gsub("(ciliate\\b)|(ciliates\\b)", "Ciliophora", spls$search_term, perl = TRUE, ignore.case = TRUE)
  spls$search_term <- gsub("(\\bflagellates\\b)|(\\bflagellates$)|(\\buniflagellatae\\b)|(\\bunifagellatae\\b)|(\\bbiflagellatae\\b)|(\\bfourflagellates\\b)", "flagellate", spls$search_term, perl = TRUE, ignore.case = TRUE)
  spls$search_term <- gsub("(choanoflagellate\\b)|(\\bchoanoflagellates\\b)", "Choanoflagellatea", spls$search_term, perl = TRUE, ignore.case = TRUE)
  spls$search_term <- gsub("(coccolithophores)|(coccolithophore)", "Coccolithales", spls$search_term, perl = TRUE, ignore.case = TRUE)
  spls$search_term <- gsub("(naviculoid)", "Navicula", spls$search_term, perl = TRUE, ignore.case = TRUE)
  spls$search_term <- gsub("(scuticociliophora)", "Scuticociliatia", spls$search_term, perl = TRUE, ignore.case = TRUE)
  spls$search_term <- gsub("(dinophycean)", "Dinophyceae", spls$search_term, perl = TRUE, ignore.case = TRUE)
  spls$search_term <- gsub("(thalassiotrix)", "Thalassiothrix", spls$search_term, perl = TRUE, ignore.case = TRUE)
  
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
  
  # Remove lonely punctuation characters 
  spls$search_term <- gsub("\\s\\W$", "", spls$search_term, perl = TRUE) 
  
  # Remove search terms that contain only one character
  spls$search_term[nchar(spls$search_term) <= 2] <- NA
  
  # Replace unknowns and vague entries by NA
  spls$certainty[tolower(spls$search_term) %in% c("unknown", "incertain taxa", "incertains taxa", "unknown taxon")] <- "unknown"
  
  spls$search_term[tolower(spls$search_term) %in% c("unknown", "incertain taxa", "incertains taxa", "unknown taxon", "fecal pellets", "algae", "bacteria", "green globul", "green ovoides", "coccoid cells", "encysting protist on stem", "cocal sphere")] <- NA
  
  # remove extra white space at the end of some words (unknown format, trimws does not always work)
  spls$search_term <- gsub("[[:space:]]$", "", spls$search_term)
  
  # only first word as capital letter
  spls$search_term <- gsub("(^\\w)", "\\U\\1", tolower(spls$search_term), perl=TRUE)
  
  ## Change species names that the search cannot handle

  spls$search_term[spls$search_term %in% "Gymnodinium wulfii"] <- "Gymnodinium wulffii"
  spls$search_term[spls$search_term %in% "Myrionecta rubra"] <- "Mesodinium rubrum"
  
  # remove extra white space once more
  spls$search_term <- trimws(spls$search_term) 
  
  
  ## Species list column classes
  
  spls <- rapply(spls, factor, classes = "character", how = "replace")
  
  ## return the species list ####
  out <- NULL
  out$data <- spls
  out$sum_info <- sum.info
  class(out) <- "speciesList"
  out
}
