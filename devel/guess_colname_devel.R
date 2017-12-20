cols1 <- c("Expedition", "Sample.name", "Station", "Latitude.(degrees)", "Latitude.(minutes)", "Latitude.(decimals)", "Longitude.(degrees)", 
"Longitude.(minutes)", "Longitude.(decimals)", "Bottom.depth.(m)", 
"Sampling.date.(UTC)", "Gear", "Sampling.depth.(m).from", "Sampling.depth.(m).to", 
"Filtered.volume", "Sample.type", "Responsible.person", "Comment"
)

cols2 <- c("ID", "Expedition", "Station", "Sampled.from", "Cast", "Transect", 
"gps", "Latitude.(deg)", "Latitude.(sec)", "Latitude.(decimals)", 
"Longitude.(degr", "Longitude.(deg)", "Longitude.(decimals)", 
"Bottom.depth.(m)", "lon.utm", "lat.utm", "dist.krone", "dist.conway", 
"dist.kongs", "Tow.distance.(m)", "Deployment.time.(days)", "Sampling.date.(UTC)", 
"Gear", "Sampling.depth.(m).from", "Sampling.depth.(m).to", "Sample.type", 
"Sample.name", "Filtration.volume.(ml)", "Salinity", "Temp.", 
"pH", "Contact.person", "Comment", "Beröe.cucumis.picked.out", 
"Mertensia.ovum.picked.out", "Indet.gelatinous.picked.out")

cols3 <- c("Expedition", "Sample.name", "Program", "Ship", "Floe", "Station", 
"Cast.number", "Latitude..decimals.", "Longitude..decimals.", 
"Bottom.depth..m.", "Sampling.date..UTC.", "Gear", "Sampling.depth..m..from.Distance.from.core.bottom..cm..from", 
"Sampling.depth..m..to..Distance.from.core.bottom..cm..to", "Animal.group", 
"Sample.type", "Contact.person", "Comment", "Ice.core.number", 
"Ice.thickness..cm.", "Snow.thickness..cm.", "Freeboard..cm.", 
"Total.melted.volume..ml.", "Salinity", "Filtered.volume..ml.", 
"Comment.Anette", "X27", "X28", "X29", "X30", "X31")

required_cols <- c("expedition", "station", "type", "sample_name",
 "longitude", "latitude", "date", "bottom_depth", "gear", "from", "to",
 "filtered_volume", "responsible", "comment")

search_words <- function(column) {
  switch (column,
  expedition = "expedition",
  sample_name = "name",
  station = "station",
  latitude = "latitude decimal",
  longitude = "longitude decimal",
  bottom_depth = "bottom depth",
  date = "date",
  gear = "gear",
  from = "depth m from",
  to = "depth m to",
  filtered_volume = "(filtered|filtration).volume",
  type = "type",
  responsible = "person",
  comment = "comment",
  stop(paste(column, "column type not set"))
)
}

agrep("(filtered|filtration).volume", gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", gsub("[[:punct:]]", " ", cols1), perl = TRUE), ignore.case = TRUE, value = TRUE, max = 5, fixed = FALSE)[1]

grep("(filtered|filtration).volume", gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", gsub("[[:punct:]]", " ", cols3), perl = TRUE), ignore.case = TRUE, value = TRUE, perl = TRUE)

guess_colname <- function(cols = required_cols, original_cols, candidates = search_words) {
  sapply(cols, function(k) {
grep(candidates(k), gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", gsub("[[:punct:]]", " ", original_cols), perl = TRUE), ignore.case = TRUE, perl = TRUE, value = TRUE)[1]
})
}


guess_colname <- function(cols = required_cols, original_cols, candidates = search_words) {
  sapply(cols, function(k) {
agrep(candidates(k), gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", gsub("[[:punct:]]", " ", original_cols), perl = TRUE), ignore.case = TRUE, value = TRUE, max = 5)[1]
})
}

guess_colname(original_cols = cols1)
guess_colname(original_cols = cols2)
guess_colname(original_cols = cols3)
