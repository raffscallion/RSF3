
#' CleanPolygons
#'
#' Prepares generic polygon data for use in Tranche 1 (or elsewhere).  It does the following:
#' 1) Eliminate bad topologies
#' 2) Convert to the common projection
#' 3) Add the common SF fields
#'
#' @param config A filename for the config file for the data to be cleaned
#'
#' @return An SPDF for use in further SF processing
#' @export
#'
#' @import dplyr
#'
#' @examples CleanPolygons('./config/MN_RX.R')
CleanPolygons <- function(config) {

  # Get the config info for this data source
  source(config)

  # load shapefile, find and filter bad shapes
  raw.shapes <- rgdal::readOGR(inpath, inname)
  bad.geoms <- rgeos::gIsValid(raw.shapes, byid=TRUE)
  shapes <- raw.shapes[bad.geoms==TRUE,]

  # Add the SF official fields
  # ID, startdate, enddate, type, source
  shapes@data <- mutate_(shapes@data, sf_area = AREA,
                         sf_start = START,
                         sf_end = END,
                         sf_type = TYPE,
                         sf_name = NAME,
                         sf_source = SOURCE)

  # Remove any without valid dates (future approach can be more sophisticated)
  shapes <- shapes[!is.na(shapes$sf_start),]

  # recalculate IDs and add as a field in df
  n <- length(slot(shapes, 'polygons'))
  shapes <- sp::spChFIDs(shapes, as.character(1:n))
  shapes$sf_id <- as.character(row.names(shapes))

  # Put into the central projection (CONUS Albers equal area)
  shapes.proj <- sp::spTransform(shapes, sp::CRS("+init=epsg:5070"))

#   # Export to shapefile
#   writeOGR(shapes.proj, outpath, input.name, 'ESRI Shapefile')
}

