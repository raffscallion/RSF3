# SF Configuration file

input.name <- 'FACTS'  # A friendly name for metadata and the output name
inname <- "20120410_FY11_FireTreatments.csv"
inpath <- "./InputData/FACTS/"
outpath <- './FinalData/Tranche2'

# How the SF standard fields should be calculated.  These are
# passed to mutate()
#ID <- "rownames(points@data)"
ID <- "row_number()"
AREA <- "Accomplished.Acres"
START <- "as.Date(strptime(Accomplished.Date, format='%Y-%m-%d'))"
END <- "as.Date(strptime(Completed.Date, format='%Y-%m-%d'))"
TYPE <- "'RX'"
NAME <- "NA"
SOURCE <- "'FACTS'"

# Point data (csv) specific parameters
within.distance <- 500   # Distance in meters considered associated with the primary poly (should relate to spatial uncertainty)
columns <- c(NA, NA, NA, NA, NA, NA, NA, NA, 'numeric', 'numeric',
             NA, NA, NA, NA, NA, NA, NA)
coord.fields <- c("LONGITUDE", "LATITUDE")
