# SF Configuration file

input.name <- 'MN_DNR_WF'  # A friendly name for metadata and the output name
inname <- "2011 MN DNR Wildfires.csv"
inpath <- "./InputData/MN/"
outpath <- './FinalData/Tranche2'

# How the SF standard fields should be calculated.  These are
# passed to mutate()
#ID <- "rownames(points@data)"
ID <- "row_number()"
AREA <- "total_acres"
START <- "as.Date(strptime(discovery_date, format='%m/%d/%Y'))"
END <- "as.Date(strptime(fire_out_date, format='%m/%d/%Y'))"
TYPE <- "'WF'"
NAME <- "fire_number"
SOURCE <- "'MN_DNR_WF'"

# Point data (csv) specific parameters
within.distance <- 500   # Distance in meters considered associated with the primary poly (should relate to spatial uncertainty)
columns <- c(NA, NA, NA, NA, NA,NA, NA, NA, NA, NA, NA, NA, NA, 'numeric', 'numeric')
coord.fields <- c("longitude", "latitude")
#input.file <- './InputData/MN/2011 MN DNR Wildfires.csv'
#output.path <- './FinalData/Tranche2'
