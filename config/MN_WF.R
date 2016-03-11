# SF Configuration file

input.name <- 'MN_WF'  # A friendly name for metadata and the output name
inname <- "2011_wildfire"
inpath <- "./InputData/MN"
outpath <- "./InputData/Tranche1"

# How the SF standard fields should be calculated.  These are
# passed to mutate()
AREA <- "ACRES"
START <- "as.Date(DATE, format='%m/%d/%Y')"
END <- "as.Date(DATE, format='%m/%d/%Y')"
TYPE <- "'WF'"
NAME <- "'unknown'"
SOURCE <- "'MN_WF'"
