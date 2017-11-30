### Dataset exporting

source("runfirst.R")

library(openxlsx)

## Sample types

TYPES <- read.xlsx("devel/Sample_types.xlsx", 1)

save(TYPES, file = "Sample_types.rda")
