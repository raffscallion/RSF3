# SF Configuration file

input.name <- 'MN_FWS'  # A friendly name for metadata and the output name
inname <- "FMIS_MN_2012.csv"
inpath <- "./InputData/MN/"
outpath <- './FinalData/Tranche2'

# How the SF standard fields should be calculated.  These are
# passed to mutate()
#ID <- "rownames(points@data)"
ID <- "row_number()"
AREA <- "TOTALACRES"
START <- "as.Date(strptime(STARTDATE, format='%m/%d/%Y'))"
END <- "as.Date(strptime(STARTDATE, format='%m/%d/%Y'))"
TYPE <- "ifelse(FIRETYPE == 'TREATMENT', 'RX', 'WF')"
NAME <- "FIRENAME"
SOURCE <- "'MN_FWS'"

# Point data (csv) specific parameters
within.distance <- 500   # Distance in meters considered associated with the primary poly (should relate to spatial uncertainty)
columns <- c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 'numeric', 'numeric',
             NA, NA, NA, NA, NA)
coord.fields <- c("LONGITUDE", "LATITUDE")
