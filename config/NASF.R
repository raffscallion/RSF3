# SF Configuration file

input.name <- 'NASF'  # A friendly name for metadata and the output name
inname <- "2011_NASF_clean.csv"
inpath <- "./InputData/NASF/"
outpath <- './FinalData/Tranche2'

# How the SF standard fields should be calculated.  These are
# passed to mutate()
#ID <- "rownames(points@data)"
ID <- "row_number()"
AREA <- "Final_Fire_Acre_Quantity"
START <- "as.Date(strptime(Fire_Discovery_Date, format='%m/%d/%Y'))"
END <- "as.Date(strptime(EndDate, format='%m/%d/%Y'))"
TYPE <- "'WF'"
NAME <- "Incident_Name"
SOURCE <- "'NASF'"

# Point data (csv) specific parameters
within.distance <- 500   # Distance in meters considered associated with the primary poly (should relate to spatial uncertainty)
columns <- c(NA, NA, NA, NA, NA,NA, NA, NA, NA, NA, NA, NA, NA, NA, 'numeric', 'numeric',
             NA, NA, NA, NA, NA,NA, NA, NA, NA, NA, NA, NA, NA)
coord.fields <- c("Longitude", "Latitude")
