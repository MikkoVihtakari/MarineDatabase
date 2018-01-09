#' @title Confirm species names from the World Register of Marine Species
#' @description Uses the \code{\link[=taxize]{taxize}} package to confirm a \link[=make_species_list]{species list} against the World Register of Marine Species database. 
#' @param x A \code{\link[=make_species_list]{speciesList}} object.
#' @param ranks Ranks to be included in the output (see \code{Value})
#' @author Mikko Vihtakari
#' @import taxize
#' @export

#ranks = c("kingdom", "phylum", "class", "order", "genus")
confirm_species_list <- function(x, ranks = c("kingdom", "phylum", "class", "order", "genus")) {

## Data checks 
  
if(class(x) != "speciesList") stop("The function requires a speciesList object. See ?make_species_list")

## Database name conversion ###

databases <- function(db) {
  switch(db,
  ITIS = "itis",
  NCBI = "ncbi",
  `World Register of Marine Species` = "worms",
  EOL = "eol",
  `Union 4` = "union4",
  `Not found` = NA,
  stop(db, "not implemented"))
}

## Database search ###

spls <- x$data

sps <- unique(as.character(spls$search_term))

sps <- sps[!is.na(sps)]

## Change species names that the search cannot handle

sps[sps %in% "Gymnodinium wulfii"] <- "Gyrodinium wulffii"
sps[sps %in% "Myrionecta rubra"] <- "Mesodinium rubrum"

## Order alphabetically

sps <- sps[order(sps)]

#c(9,12,3,4,7,11)

## gnr_resolve all species names

resolved <- taxize::gnr_resolve(sps, data_source_ids = 9,
                                best_match_only = TRUE,    
                                with_context = TRUE,
                                canonical = TRUE,
                                fields = "all")


resolved <- resolved[c("user_supplied_name", "data_source_title", "matched_name2", "taxon_id")]

nonresolved <- sps[!sps %in% resolved$user_supplied_name]

if(length(nonresolved) != 0) {
  add2resolved <- taxize::gnr_resolve(nonresolved, data_source_ids = 3,
                                    best_match_only = TRUE,
                                    with_context = TRUE,
                                    canonical = TRUE,
                                    fields = "all")
if(length(add2resolved) != 0) {
  resolved <- rbind(resolved, add2resolved[c("user_supplied_name", "data_source_title", "matched_name2", "taxon_id")])
}
}

resolved$data_source_title <- sapply(resolved$data_source_title, function(k) databases(k))
resolved$matched_name2[resolved$matched_name2 == ""] <- NA

nonresolved <- sps[!sps %in% resolved$user_supplied_name]

resolved <- rbind.fill(resolved, data.frame(user_supplied_name = nonresolved))

resolved <- resolved[order(resolved$user_supplied_name),]
rownames(resolved) <- 1:nrow(resolved)

sp_list <- split(resolved, resolved$user_supplied_name)
 
## Find species IDs from the databases an extract information

k <- sp_list[[33]] 

search_out <- lapply(sp_list, function(k) {
  message(paste(k$user_supplied_name, "i =", which(resolved$user_supplied_name == k$user_supplied_name)))
  
  ## Not found species entries
    if(any(is.na(k$matched_name2))) {
   out <- data.frame(search_term = k$user_supplied_name)
      attributes(out)$warning <- paste("No matching species name found!")
  } else {
    
  ## WORMS
    
  if(any(k$data_source_title %in% "worms")) {
   k <- subset(k, data_source_title == "worms")
   
   ## ID
   
   if(k$user_supplied_name == "Navicula") { # exception to fix Navicula being matched as a diatom
     tmp <- as.data.frame(get_wormsid("Navicula", accepted = TRUE, row = 1))
   } else {
    tmp <- as.data.frame(taxize::as.wormsid(k$taxon_id))  
   }
     
   #tmp <- as.data.frame(taxize::get_wormsid(k$matched_name2, accepted = TRUE, rows = 1))
   url <- tmp$uri
   id <- tmp$ids
   Rnk <- taxize::tax_rank(id, db = "worms")[[1]]
   
   ## Classification
   cl <- as.data.frame(taxize::classification(id, db = "worms")[[1]])
   
   ## Extract ranks
   
    rnk <- lapply(ranks, function(g) {
    out <- data.frame(cl[which(tolower(cl$rank) == g), "name"])
      if(nrow(out) == 0) out <- data.frame(NA)
    colnames(out) <- g
    out
    })
    
    Ranks <- do.call(cbind, rnk)

out <- cbind(data.frame(search_term = k$user_supplied_name,
  matched_name = k$matched_name2,
  db = "worms", 
  rank_level = Rnk),
  Ranks,
  data.frame(id = id, url = url))
    
  } else {
    
  ## ITIS 
    
  if(any(k$data_source_title %in% "itis"))
   k <- subset(k, data_source_title == "itis")    
  
    tmp <- as.data.frame(taxize::as.tsn(k$taxon_id))
    url <- tmp$uri
    id <- tmp$ids
    Rnk <- taxize::tax_rank(id, db = "itis")[[1]]
   
   ## Classification
   cl <- as.data.frame(taxize::classification(id, db = "itis")[[1]])
   
   ## Extract ranks
   
    rnk <- lapply(ranks, function(g) {
    out <- data.frame(cl[which(tolower(cl$rank) == g), "name"])
      if(nrow(out) == 0) out <- data.frame(NA)
    colnames(out) <- g
    out
    })
    
    Ranks <- do.call(cbind, rnk)

out <- cbind(data.frame(search_term = k$user_supplied_name,
  matched_name = k$matched_name2,
  db = "itis", 
  rank_level = Rnk),
  Ranks,
  data.frame(id = id, url = url))
  
  }}

  ## Return the data
  out
  
})
 
OUT <- do.call(rbind.fill, search_out)

OUT
}
 
