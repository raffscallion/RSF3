# SF Configuration file for Tranche 3 Data
# VIIRS i-band

input.name <- 'VIIRS-i'  # A friendly name for metadata and the output name
inname <- "VIIRS_i_2015_MN_noAg.txt"
inpath <- "./InputData/VIIRS_I/"
outpath <- './FinalData/Tranche3'

# How the SF standard fields should be calculated.  These are
# passed to mutate()
#ID <- "rownames(points@data)"
ID <- "row_number()"
AREA <- 50
START <- "as.Date(strptime(DATE, format='%m/%d/%Y'))"
END <- "as.Date(strptime(DATE, format='%m/%d/%Y'))"
TYPE <- "'unknown'"
NAME <- "'unknown'"
SOURCE <- "'VIIRS-i'"

# Point data (csv) specific parameters
within.distance <- 1500   # Distance in meters considered associated with the primary poly (should relate to spatial uncertainty)
columns <- c(NA, NA, NA, NA, NA, 'numeric', 'numeric', NA, NA, NA, NA, NA, NA, NA, NA)
coord.fields <- c("LONG", "LAT")

# Size (in acres) to assume for a pixel in the absence of anything else
NOMINAL.SIZE = 35 # just larger than nadir resolution (30.27 acres)

# Nominal nadir pixel resolution (in meters)
PIXEL.RESOLUTION <- 350
