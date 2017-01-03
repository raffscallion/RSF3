---
title: "RSF3 Overview"
author: "Sean Raffuse"
date: "2017-01-03"
output: rmarkdown::html_vignette
---

RSF3 is an updated implementation of the SmartFire (SF) fire information reconciliation algorithms. Both SF1 and SF2 were designed as enterprise operational systems to support national, real-time smoke modeling. RSF3 is a lightweight implementation of core SF algorithms only. The goal of RSF3 is to provide a collaborative and extensible package for processing fire activity data for use in retrospective emission inventories. RSF3 also rethinks and updates the algorithms in SF2. The biggest change is the concept of *tranches*.

### Tranches

Processing in RSF3 is divided into tranches. Tranche is a French word meaning "slice" or "portion" and is used in the finance world. Here, we divide fire input types into tranches, which are levels of data quality and precision.

1. Tranche 1 includes only polygon data sets. Fire locations derived from polygons are the most precise.
2. Tranche 2 includes point data sets on specific known fires, such as incident reports or accomplishments from a state prescribed burn database.
3. Tranche 3 includes point data sets that are not associated with specific fires. These are predominantly satellite hot spot data sets.

RSF3 processes the tranches in sequence. If a fire is available in tranche 1 data, overlapping fires from tranches 2 and 3 are excluded. Tranche 3 data is used only if there are no reconcilable fires from tranches 1 and 2.

### General Processing Steps

The following list outlines the general steps used to create a reconciled fire activity data set using RSF3. An example of these steps is shown in TestRSF3.R.

1. Identify all input data sets as either tranche 1, 2, or 3. Tranche 1 data should be in ESRI Shapefile format. Tranches 2 and 3 should be CSV files.
2. Create configuration files for each input data set (see next section).
3. Run `InputTranche1` for each tranche 1 data set. This puts all of the data sets into a common format for further processing. Note that data from Geomac require special preprocessing and `InputGeomac` is run instead of `InputTranche1`.
4. Run `ProcessTranche1` to merge all tranche 1 data sets into a single output *T1*.
5. Run `InputTranche2` for each tranche 2 data set.
6. Run `ProcessTranche2` to merge all tranche 2 data sets into a single output *T2*.
7. Run `MergeTranches` to combine *T1* and *T2* into *T1.T2*.
8. Run `InputHMS` to create tranche 3 (*T3*). Note that HMS is the only supported tranche 3 data set at the time of writing.
9. Run `MergeTranches` to combine *T1.T2* and *T3* into the final result.
10. Save the final result as a shapefile for use in other applications using `writeOGR`.

### Configuration Files

Processing of individual input data sets is driven by configuration files. This allows for mostly painless processing of new data sets. The configuration file consists of R code. Here is a config file for a downloaded shapefile of MTBS burn polygons with comments explaining each record.

```
# SF Configuration file for Tranche 1

input.name <- 'MTBS'                # A friendly name for metadata and the output name
inname <- "MTBS_2011"               # The filename without extension
inpath <- "./InputData/MTBS"        # Location of the file
outpath <- "./InputData/Tranche1"   # Target location for output file

# How the SF standard fields should be calculated.  These are passed to mutate()
AREA <- "Acres"     # Use the value from the field called Acres
START <- "as.Date(paste(Year, StartMonth, StartDay, sep = '-'), format='%Y-%m-%d')"  # Start date must be in Date format. Value computed from fields
END <- "sf_start"   # Use the start date computed above
TYPE <- "FireType"  # Use the value from the field called FireType
NAME <- "Fire_Name" # Use the value from the field called Fire_Name
SOURCE <- "'MTBS'"  # Where did this data come from? Notice the nested quotes to pass a string literal.
```

Tranche 2 (point) data require a few more items be specified. Here is a config file for a CSV data from US FWS for the state of Minnesota.

```
# SF Configuration file for Tranche 2

input.name <- 'MN_FWS'              # A friendly name for metadata and the output name
inname <- "FMIS_MN_2012.csv"        # The filename without extension
inpath <- "./InputData/MN/"         # Location of the file
outpath <- './FinalData/Tranche2'   # Target location for output file

# How the SF standard fields should be calculated.  These are passed to mutate()
ID <- "row_number()"                                        # An ID is required for Tranche 2. This just provides a unique number.
AREA <- "TOTALACRES"                                        # Use the value from the TOTALACRES field
START <- "as.Date(strptime(STARTDATE, format='%m/%d/%Y'))"  # Must be in Date format. Use the STARTDATE field.
END <- "as.Date(strptime(STARTDATE, format='%m/%d/%Y'))"    # Same as START
TYPE <- "ifelse(FIRETYPE == 'TREATMENT', 'RX', 'WF')"       # Calculate from FIRETYPE field
NAME <- "FIRENAME"                                          # Use the value from FIRENAME field
SOURCE <- "'MN_FWS'"                                        # String literal

# Point data (csv) specific parameters
within.distance <- 500   # Distance in meters considered associated with the primary poly (should relate to spatial uncertainty)
# This vector should have a value for each field in the original CSV.  It must be 'numeric' for the latitude and longitude columns
# and can be NA for all other columns.
columns <- c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 'numeric', 'numeric',
             NA, NA, NA, NA, NA)
coord.fields <- c("LONGITUDE", "LATITUDE")  # Which fields provide latitude and longitude information. Must be in decimal degrees.
```

Tranche 3 config files look like tranche 2 files with one additional record specifying a default fire size. Here is the HMS config.

```
# SF Configuration file for Tranche 3

input.name <- 'HMS'  # A friendly name for metadata and the output name
inname <- "HMS_2011_MN.csv"
inpath <- "./InputData/HMS/"
outpath <- './FinalData/Tranche3'

# How the SF standard fields should be calculated.  These are
# passed to mutate()
ID <- "row_number()"
AREA <- 100
START <- "as.Date(strptime(YearDay, format='%Y%j'))"
END <- "as.Date(strptime(YearDay, format='%Y%j'))"
TYPE <- "'unknown'"
NAME <- "'unknown'"
SOURCE <- "'HMS'"

# Point data (csv) specific parameters
within.distance <- 3000   # Distance in meters considered associated with the primary poly (should relate to spatial uncertainty)
columns <- c('numeric', 'numeric', NA, NA, NA, NA, NA)
coord.fields <- c("Lon", "Lat")

# Size (in acres) to assume for a pixel in the absence of anything else
NOMINAL.SIZE = 100
```
