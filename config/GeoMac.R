# SF Configuration file

input.name <- 'GeoMac'  # A friendly name for metadata and the output name
inname <- "2011_perimeters_dd83"
inpath <- "./InputData/GeoMac"
outpath <- "./InputData/Tranche1"

# How the SF standard fields should be calculated.  These are
# passed to mutate()
AREA <- "acres"
START <- "as.Date(first.date)"
END <- "as.Date(date_, format='%Y/%m/%d')"
TYPE <- "'WF'"
NAME <- "fire_name"
SOURCE <- "'GeoMac'"
ID <- "as.character(ID)"
