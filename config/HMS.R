# SF Configuration file

input.name <- 'HMS'  # A friendly name for metadata and the output name
inname <- "HMS_2011_MN_noAg.csv"
inpath <- "./InputData/HMS/"
outpath <- './FinalData/Tranche3'

# How the SF standard fields should be calculated.  These are
# passed to mutate()
#ID <- "rownames(points@data)"
ID <- "row_number()"
AREA <- 100
START <- "as.Date(strptime(YearDay, format='%Y%j'))"
END <- "as.Date(strptime(YearDay, format='%Y%j'))"
TYPE <- "'unknown'"
NAME <- "'unknown'"
SOURCE <- "'HMS'"

# Point data (csv) specific parameters
within.distance <- 3000   # Distance in meters considered associated with the primary poly (should relate to spatial uncertainty)
columns <- c('numeric', 'numeric', NA, NA, NA, NA, NA, NA, NA, NA)
coord.fields <- c("Lon", "Lat")

# Size (in acres) to assume for a pixel in the absence of anything else
NOMINAL.SIZE = 100
