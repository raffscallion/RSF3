
#' InputHMS
#'
#' Prepares generic HMS data for use in Tranche 3. This does the following:
#' 1) Convert to standard projection and add common fields
#' 2) Intersect with T1 and T2 to segregate new data
#' 3) Convert to polys and cluster
#' 4) Determine start and end dates for each cluster
#' 5) For single pixel clusters, rescale based on neighborhood size
#'
#' @param config A config file with input parameters
#' @param T1T2 SPDF The preprocessed tranche 1 and 2 polygons
#'
#' @return SPDF
#' @export
#' @import dplyr
#'
#' @examples InputHMS('./config/HMS.R', T1.T2)
InputHMS <- function(config, T1T2) {

  # Simplification tolerance (in meters)
  tolerance <- 20
  m2.per.acre <- 4046.856

  # Load configuration
  source(config, local = TRUE)

#   # Required packages
#   library(rgdal)
#   library(rgeos)
#   library(dplyr)
#   library(spatstat)


  # Get points
  input.file <- paste0(inpath, inname)
  points.csv <- read.csv(input.file, stringsAsFactors=FALSE, colClasses=columns)
  # Promote to spatial
  points <- sp::SpatialPointsDataFrame(coords = points.csv[, coord.fields], data = points.csv)
  # Add WGS-84 coordinate system (assumed if we simply have lat/lon csv data)
  proj4string(points) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  # Put into the central projection (CONUS Albers equal area)
  points <- sp::spTransform(points, sp::CRS("+init=epsg:5070"))
  # Add the SF official fields
  # ID, startdate, enddate, type, source
  points@data <- mutate_(points@data,
                        sf_id = ID,
                        sf_area = AREA,
                        sf_start = START,
                        sf_end = END,
                        sf_type = TYPE,
                        sf_name = NAME,
                        sf_source = SOURCE) %>%
    mutate(sf_id = as.character(sf_id))


  ### 1) Intersect and eliminate points associated with T1 and T2 polys

  # Get T1 and T2 polygons
  # t.polys <- readOGR(dsn='./FinalData/Merged', layer='Tranches1and2', stringsAsFactors=FALSE)
  # Running gSimplify before all of this to improve performance.
  p.polys <- rgeos::gSimplify(T1T2, tolerance, topologyPreserve=TRUE)
  # buffer the polys
  poly.buffered <- rgeos::gBuffer(p.polys, byid=TRUE, width=within.distance)
  # intersect
  ints <- rgeos::gIntersects(points, poly.buffered, byid=TRUE)
  ints.collapse <- apply(ints, 2, function(x) {sum(x)})
  # Split into two outputs, those within threshold and those without
  unmatched <- points[ints.collapse == 0,]
  matched <- points[ints.collapse > 0,]
#   # We should capture these matched IDs for later processing or joining with final data
#   writeOGR(matched, paste(output.path,'matched',sep='/'), paste0(input.name, '_matched') , 'ESRI Shapefile')


  ### 2) Cluster
  # Convert to planar coordinates (HMS specific)
  proj <- "+proj=lcc +lat_1=20.0 +lat_2=70.0 +lon_0=-105.0 +k_0=1.0 +x_0=0.0 +y_0=0.0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
  projected <- sp::spTransform(unmatched, CRS=sp::CRS(proj))
  # get the coordinates out
  coords <- as.data.frame(projected@coords)
  # add a unique ID to each row
  coords["id"] <- seq(from=1, to=length(coords[,1]))

  #Apply the square function on the array using apply (making slightly larger than 1km to ensure no gaps)
  square.half.length <- 525
  polys <- apply(coords, 1, squarePolygon, square.half.length)
  # wrap into a polygons, and then spatialpolygons object
  sps <- sp::SpatialPolygons(polys)
  # add the projection definition
  proj4string(sps) = sp::CRS(proj)
  # now we can attach the original data to the polygons
  spdf <- sp::SpatialPolygonsDataFrame(sps, projected@data, match.ID=FALSE)
#   # Let's export a shapefile for posterity
#   writeOGR(spdf, output.path, "HMS_footprint_MN_2011", driver="ESRI Shapefile")

  # Now buffer into clusters, finding min and max date
  # source("./Code/ClusterFootprints.R")
  clustered <- clusterFootprints(spdf)
  # clusterFootprints returns fake polygons where holes should be. These can be
  # detected and removed because the data attributes are NA
  clustered <- clustered[!is.na(clustered$sf_source),]


  ### 3) Rescale single-pixel fires
  # Combine T1_T2 polys and HMS single-pixel fires into one point pattern object (ppp)
  # with marks for the polygon area and NA for HMS area, then calculate nearest neighbor
  # area for each - this will give the new areas for the HMS data for rescaling

  # Split into single pixel and multi-pixel fires
  single.pixel.size <- (((square.half.length * 2)^2) / m2.per.acre) + 1 # adding 1 just in case
  single.pixels <- clustered[clustered@data$sf_area <= single.pixel.size,]
  multi.pixel  <- clustered[!clustered@data$sf_area <= single.pixel.size,]

  # Convert polys to a df for ppp (X, Y, area)
  prepPPP <- function(points) {
    centroids <- coordinates(points)
    points$lon <- centroids[,1]
    points$lat <- centroids[,2]
    df <- points@data
    df <- select(df, lon, lat, sf_area)
  }

  ### Do all cluster processing in HMS projection,
  #need to project original T2 poly data, but first make points to speed projection
  #gCentroid
  t.points <- ConvertPolyToPoint(T1T2)
  t.points <- sp::spTransform(t.points, CRS=sp::CRS(proj))
  t.points <- prepPPP(t.points)
  hms.points <- ConvertPolyToPoint(single.pixels)
  hms.points <- prepPPP(hms.points)
  hms.points$sf_area <- NA
  points.ppp <- rbind(hms.points, t.points)  # put HMS first so they are easier to pull out
#
#   points.polys <- prepPPP(t.polys)
#   points.hms <- prepPPP(single.pixels)
#   points.hms$sf_area <- NA
#   points.ppp <- rbind(points.hms, points.polys) # put HMS first so they are easier to pull out
  # Create a window using the min and max (not the best for spatial statistics, but fine for us)
  x.min <- min(points.ppp$lon)
  x.max <- max(points.ppp$lon)
  y.min <- min(points.ppp$lat)
  y.max <- max(points.ppp$lat)
  window <- spatstat::as.owin(c(x.min, x.max, y.min, y.max))
  ppp <- spatstat::as.ppp(points.ppp, W=window)

  # # Calculate the median size of all fires within a 50 km neighborhood
  # median.size <- spatstat::markstat(ppp, median, R=50000, na.rm=TRUE)

  # Calculate the median size of the 10 nearest fires
  median.size <- spatstat::markstat(ppp, median, N=10, na.rm=TRUE)
  # Grab only the medians for HMS data
  median.size <- median.size[1:length(hms.points$lon)]

  # Where median size is not NA and < single pixels size, replace current value
  hms.points <- mutate(hms.points, sf_area =
                         ifelse(!is.na(median.size) & median.size < single.pixel.size,
                                median.size,
                                NOMINAL.SIZE))
  # Now need to recreate the single pixel fires as polygons and merge with the multi-pixel
  # Calculate the square half length (in meters) for passing to the squarePolygon function
  hms.points <- mutate(hms.points, square.half = ((sf_area * m2.per.acre)^0.5)/2,
                       id = row_number())

  # Turn back into square polygons with size based on the medians calculated above
  polys <- mapply(squarePolygonMulti, hms.points$lon, hms.points$lat, hms.points$id, hms.points$square.half)
  sps <- SpatialPolygons(polys)
  # add the projection definition
  proj4string(sps) = sp::CRS(proj)
  # now we can attach the original data to the polygons and combine with clusters
  df <- single.pixels@data
  df$sf_area <- hms.points$sf_area
  spdf <- sp::SpatialPolygonsDataFrame(sps, df, match.ID=FALSE)

  # Need to create unique ids to combine the two data sets
  n.clusters <- length(multi.pixel$sf_id)
  multi.pixel <- sp::spChFIDs(multi.pixel, as.character(seq(1, n.clusters)))
  n.pixels <- length(spdf$sf_id)
  spdf <- sp::spChFIDs(spdf, as.character(seq(n.clusters+1, n.clusters+n.pixels)))
  final.hms <- maptools::spRbind(multi.pixel, spdf)

  #Reproject back to SF standard projection
  final.hms <- sp::spTransform(final.hms, CRS=sp::CRS("+init=epsg:5070"))

#   # Write output as shapefile and RDS
#   writeOGR(final.hms, output.path, paste(input.name, 'Tranche3', sep='_'), 'ESRI Shapefile')
#   saveRDS(final.hms, file = paste0(output.path, '/', input.name, '_Tranche3.RDS'))

}
