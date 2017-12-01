### Dataset exporting

source("runfirst.R")

library(openxlsx)

## Sample types

TYPES <- read.xlsx("devel/Sample_types.xlsx", 1)

save(TYPES, file = "data/sample_types.rda")

GEAR <- read.xlsx("devel/GEAR_types.xlsx", 1)

save(GEAR, file = "data/gear_types.rda")
