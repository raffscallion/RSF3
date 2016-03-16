# SF Configuration file

input.name <- 'MTBS'  # A friendly name for metadata and the output name
inname <- "MTBS_2011"
inpath <- "./InputData/MTBS"
outpath <- "./InputData/Tranche1"

# How the SF standard fields should be calculated.  These are
# passed to mutate()
AREA <- "Acres"
START <- "as.Date(paste(Year, StartMonth, StartDay, sep = '-'), format='%Y-%m-%d')"
END <- "sf_start"
TYPE <- "FireType"
NAME <- "Fire_Name"
SOURCE <- "'MTBS'"
