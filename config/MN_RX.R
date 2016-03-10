# SF Configuration file

input.name <- 'MN_RX'  # A friendly name for metadata and the output name
inname <- "rxburns_2011"
inpath <- "./InputData/MN"
outpath <- "./InputData/Tranche1"

# How the SF standard fields should be calculated.  These are
# passed to mutate()
AREA <- "ACRES"
START <- "as.Date(DATE_BURNE, format='%m/%d/%Y')"
END <- "as.Date(DATE_BURNE, format='%m/%d/%Y')"
TYPE <- "'RX'"
NAME <- "'unknown'"
SOURCE <- "'MN_RX'"
