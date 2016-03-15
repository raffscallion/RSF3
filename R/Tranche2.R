
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
#' @import dplyr
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

  # Convert sf_id to character (not sure why this didn't work on initial mutate_)
  points@data <- mutate(points@data, sf_id = as.character(sf_id))

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

#' ProcessTranche2
#'
#' Combines a list of Tranche 2 polygons into a single Tranche 2 output.  May be refactored with ProcessTranche1.  It
#' 1. Looks for intersecting polygons
#' 2. Creates a crosswalk list for later use (possibly)
#' 3. For each intersection, determine if they are within date range
#' 4. Appends all information from lower rank data to a blob in high rank data
#' 5. Write out to a constant format for later processing
#'
#' @param inputs A list of input SPDFs, already prepared by InputTranche2().  Earliest listed datasets take precedence
#'
#' @return SPDF The Tranche 2 polygons
#' @export
#'
#' @examples ProcessTranche2(list(MN_DNR_WF, MN_FWS, NASF, FACTS))
ProcessTranche2 <- function(inputs) {
  # The list of reprocessed Tranche1 datasets (in same path)
  # The earliest listed dataset takes precedence over subsequent data sets
#   inputs <- list('MN_DNR_WF_Tranche2', 'MN_FWS_Tranche2', 'NASF_Tranche2', 'FACTS_Tranche2')
#   # The location of the input Tranche1 datasets
#   inpath <- "./FinalData/Tranche2/"
#   outpath <- "./FinalData/Tranche2"
#   outname <- 'Tranche2Polygons'

#   # Required packages
#   library(rgdal)
#   library(rgeos)
#   library(dplyr)

#   # Load shapefiles to a list (previously saved as RDS files)
#   datasets <- lapply(inputs, function(x) {readRDS(paste0(inpath, x, '.RDS'))})

  # Look for intersecting polygons across all layers two at a time (Magic!)
  combos <- combn(inputs, 2, function(x) rgeos::gIntersects(x[[1]],x[[2]], byid=TRUE), simplify=FALSE)
  ints <- lapply(combos, which, arr.ind=TRUE)

  # the ints variable contains the pairs that intersect, but need to parse it to get
  # the specific data sets and ids for each intersect. That is all the mess below

  # Here are the different combinations of i and j
  n.combos <- length(ints)
  n.datasets <- length(inputs)
  x <- 1
  i <- 1
  j <- 2
  pairs <- matrix(ncol=2, nrow=n.combos)
  while (x <= n.combos) {
    pairs[x,1] <- i
    pairs[x,2] <- j
    j <- j+1
    if (j > n.datasets) {
      i <- i+1
      j <- i+1
    }
    x <- x+1
  }

  # Now walk through the list of intersections and build the list to discard
  n.intersects <- length(unlist(ints))/2
  # make a structure to store the list (ds1, dup.id1 ,ds2, dup.id2)
  candidates <- setNames(data.frame(matrix(ncol = 4, nrow = n.intersects)),c("ds1", "dup.id1", "ds2", "dup.id2"))

  i <- 1

  for (x in 1:n.combos) {
    dups.ds1 <- ints[[x]][,2]
    dups.ds2 <- ints[[x]][,1]
    dup.polys <- length(dups.ds1)
    if (dup.polys==0) next
    for (y in 1:dup.polys) {
      candidates$ds1[i] <- pairs[x,1]
      candidates$dup.id1[i] <- dups.ds1[[y]]
      candidates$ds2[i] <- pairs[x,2]
      candidates$dup.id2[i] <- dups.ds2[[y]]

      i <- i+1
    }
  }


  # Now for each candidate intersection, check to see if they intersect in time (or are close?)
  checkDates <- function(ds1, id1, ds2, id2) {
    start1 <- inputs[[ds1]][id1,]$sf_start
    end1 <- inputs[[ds1]][id1,]$sf_end
    start2 <- inputs[[ds2]][id2,]$sf_start
    end2 <- inputs[[ds2]][id2,]$sf_end
    if ((abs(start1 - start2) < 3) | (abs(end1 - end2) < 3)) {
      return(TRUE)
    } else return(FALSE)
  }
  candidates$overlap <- mapply(checkDates, candidates$ds1, candidates$dup.id1, candidates$ds2, candidates$dup.id2)

  # Now, for all candidates where overlap is true, remove the secondary data set
  toss <- filter(candidates, overlap==TRUE)

  # Remove conflicting polygons from each dataset
  ds.index <- seq(1:n.datasets)
  removeConflicts <- function(x, y) {
    toss.1 <- filter(toss, ds2 == y) %>%
      mutate(sf_id = as.character(dup.id2))
    to.keep <- anti_join(x@data, toss.1, by='sf_id') %>%
      arrange(sf_id)
    to.keep.v <- unlist(to.keep$sf_id)
    no.dup <- x[x$sf_id %in% to.keep.v,]
  }

  datasets.no.dups <- mapply(removeConflicts, inputs, ds.index)

  # Delete all non-sf fieldnames
  cleanFields <- function(x) {
    x@data <- (select(x@data, starts_with('sf_')))
    return(x)
  }

  datasets.no.dups <- mapply(cleanFields, datasets.no.dups)


  # Merge together and write to shapefile
  #source('./Code/sfUtils.R')
  newIDs<-mapply(make_sf_id, datasets.no.dups, ds.index, 2)
  merged <- do.call(rbind, newIDs)

  # change the sf_id to the rownames
  merged@data$sf_id <- as.character(row.names(merged))
  return(merged)
  # writeOGR(merged, outpath, outname, 'ESRI Shapefile')

}

