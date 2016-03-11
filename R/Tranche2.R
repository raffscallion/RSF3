
#' InputTranche2
#'
#' Formerly ProcessTranche2Dataset.R, this function assigns (associates) as many hotspots/point
#'  fire locations/reports as possible to the known fire perimeters (Tranche1) without clumping the hotspots first.
#'  Only then create via clumping or other methods a set of final/current Tranche2 polygons
#'  Assign (associate) the individual hotspots / point reports to these "Derived Polygons".
#'
#' @param config The config file for the input dataset
#' @param SPDF The Tranche 1 polygons, preprocessed in SF format
#'
#' @return SPDF
#' @export
#'
#' @examples InputTranche2('./config/MN_DNR_WF')
InputTranche2 <- function(config, T1) {

  # Simplification tolerance (in meters)
  tolerance <- 20
  m2.per.acre <- 4046.856

  # Load the configuration for this input
  source(config)

  # Get points
  input.file <- paste0(inpath, inname)
  points.csv <- read.csv(input.file, stringsAsFactors=FALSE, colClasses=columns)

  # Remove bad lat/lon
  points.csv <- points.csv[!is.na(points.csv[[coord.fields[1]]]),]

  # Make MN only for testing (special for NASF)
  #points.csv <- filter(points.csv, STATE=='Minnesota')

  # Promote to spatial
  points <- sp::SpatialPointsDataFrame(coords = points.csv[, coord.fields], data = points.csv)
  # Add WGS-84 coordinate system (assumed if we simply have lat/lon csv data)
  proj4string(points) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

  # Put into the central projection (CONUS Albers equal area)
  points <- sp::spTransform(points, sp::CRS("+init=epsg:5070"))

  # Add the SF official fields
  # ID, startdate, enddate, type, source
  points@data <-  mutate_(points@data,
                          sf_id = ID,
                          sf_area = AREA,
                          sf_start = START,
                          sf_end = END,
                          sf_type = TYPE,
                          sf_name = NAME,
                          sf_source = SOURCE)

  # Clean up NA end dates
  points@data <- mutate(points@data, sf_end = safe.if.else(is.na(sf_end), sf_start, sf_end))

#   # Get primary polygons
#   p.polys <-  readOGR(dsn='./FinalData/Tranche1', layer='Tranche1Polygons', stringsAsFactors=FALSE)

  # Calculate distance to nearest primary polygons and write out nearest ID plus distance
  # There are a couple ways to do this:
  # The slow naive approach calculates the distance matrix between all pairs.  This will not
  # scale well at all:
  ##dist <- gDistance(points, p.polys, byid=TRUE)
  # The "correct" approach would be a function that returns the nearest object in dataset B
  # for each object in dataset A.  However, this does not yet exist.  See https://stat.ethz.ch/pipermail/r-sig-geo/2013-April/018140.html
  # The best leftover approach is to specify a distance of interest, create new buffered
  # polygons that include that distance, then intersect those with the points.  This does not
  # give distance, just a binary in or out relative to the polygons.

  # Running gSimplify before all of this to improve performance.
  T1 <- rgeos::gSimplify(T1, tolerance, topologyPreserve=TRUE)

  # buffer the polys
  poly.buffered <- rgeos::gBuffer(T1, byid=TRUE, width=within.distance)

  # intersect
  ints <- rgeos::gIntersects(points, poly.buffered, byid=TRUE)
  ints.collapse <- apply(ints, 2, function(x) {sum(x)})

  # Split into two outputs, those within threshold and those without
  unmatched <- points[ints.collapse == 0,]
  matched <- points[ints.collapse > 0,]

#   # We should capture these matched IDs for later processing or joining with final data
#   writeOGR(matched, paste(output.path,'matched',sep='/'), paste0(input.name, '_matched') , 'ESRI Shapefile')
#
  # Turn unmatched data into polygons, using the area field to determine size
  buffer.sizes <- ((unmatched$sf_area * m2.per.acre)/pi)^0.5
  unmatched.polys <- rgeos::gBuffer(unmatched, byid=TRUE, width=buffer.sizes)

#   # Save the unmatched as Tranche 2 results
#   writeOGR(unmatched.polys, output.path, paste0(input.name, '_Tranche2') , 'ESRI Shapefile')
#   # Just save as R data to avoid truncation? (save as RDS to load as different name)
#   saveRDS(unmatched.polys, file = paste0(output.path, '/', input.name, '_Tranche2.RDS'))

}
