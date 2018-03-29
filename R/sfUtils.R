### sfUtils.R
#
#  Helper Functions for the SF algorithms
#


### This doesn't work at all yet
PrepForBlueSky <- function(spdf) {
  hms <- readRDS('../SmallFires/hms_processed_2014.RDS')

  longitude <- coordinates(spdf)[,1]
  latitude <- coordinates(spdf)[,2]

  bsp.in <- spdf@data %>%
    mutate(id = row_number(),
           type = 'RX',
           area = 226,
           start = format(as.Date(strptime(YearDay, format='%Y%j')), '%Y%m%d'))

  bsp.in$latitude <- latitude
  bsp.in$longitude <- longitude
  bsp.in <- select(bsp.in, id, latitude, longitude, type, area, start)

  write.csv(bsp.in, 'C:/Users/sraffuse/Google Drive/Working/RSmartFire/bspipe/ga-hms-rx.csv',
            row.names = FALSE, quote = FALSE)
}


#' MergeTranches
#'
#' Concatenates two tranches.
#'
#' @param X SPDF the first tranche to concatenate
#' @param Y SPDF the second tranche to concatenate
#'
#' @return SPDF A merged SPDF containing both sets
#' @export
#'
#' @examples MergeTranches(T1, T2)
MergeTranches <- function(X, Y) {

  # populate spids with sf_id
  X <- sp::spChFIDs(X, X$sf_id)
  Y <- sp::spChFIDs(Y, Y$sf_id)

  # Merge, IDs must be unique
  merged.polys <- maptools::spRbind(X, Y)
}

#' ProcessTranche
#'
#' Combines a list of polygon data sets into a single Tranche output. It
#' 1. Looks for intersecting polygons
#' 2. Creates a crosswalk list for later use (possibly)
#' 3. For each intersection, determine if they are within date range
#' 4. Appends all information from lower rank data to a blob in high rank data
#' 5. Write out to a constant format for later processing
#'
#' @param inputs A list of input SPDFs, already prepared by InputTranche().  Earliest listed datasets take precedence
#' @param tranche numeric The tranche number
#'
#' @return SPDF The Tranche polygons of the specified tranche number
#' @export
#'
#' @examples ProcessTranche(list(VIIRSi, HMS), 3)
ProcessTranche <- function(inputs, tranche) {

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


  # Now for each candidate intersection, check to see if they intersect in time (or are
  # close?). Making dates relatively relaxed (within 12 days) to deal with bad data
  checkDates <- function(ds1, id1, ds2, id2) {
    start1 <- inputs[[ds1]][id1,]$sf_start
    end1 <- inputs[[ds1]][id1,]$sf_end
    start2 <- inputs[[ds2]][id2,]$sf_start
    end2 <- inputs[[ds2]][id2,]$sf_end
    if ((abs(start1 - start2) < 12) | (abs(end1 - end2) < 12)) {
      return(TRUE)
    } else return(FALSE)
  }
  candidates$overlap <- mapply(checkDates, candidates$ds1, candidates$dup.id1, candidates$ds2, candidates$dup.id2)

  # Now, for all candidates where overlap is true, remove the secondary data set
  toss <- filter(candidates, overlap==TRUE)

  # Remove conflicting polygons from each dataset
  ds.index <- seq(1:n.datasets)
  removeConflicts <- function(x, y) {
    toss.1 <- filter(toss, ds2 == y) #%>%
    toss.1$sf_id <- x@data$sf_id[toss.1$dup.id2]
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
  newIDs<-mapply(make_sf_id, datasets.no.dups, ds.index, tranche)
  merged <- do.call(rbind.SpatialPolygonsDataFrame, newIDs)

  # change the sf_id to the rownames
  merged@data$sf_id <- as.character(row.names(merged))
  return(merged)

}



#' SubsetByState
#'
#' Takes a spatial data frame and returns a spatial data frame subsetted by the desired US state
#'
#' @param spd A spatial data frame (points or polygons)
#' @param state character The full state name (e.g., 'California')
#'
#' @return spd
#' @export
#'
#' @examples SubsetByState(geomac, 'CA')
SubsetByState <- function(spd, state) {
  # Load the state data and subset
  states <- rgdal::readOGR(dsn = './SupportData', layer = 'states_counties', stringsAsFactors = FALSE)
  states <- states[states$STATE_NAME == state,]
  # Subset the spatial data by the state
  states <- sp::spTransform(states, sp::CRS(sp::proj4string(spd)))
  spd <- spd[states,]
}

#' RemoveAgFires Takes a spatial data set and removes all features that overlap with
#' 'Crops' from the USDA NASS Cropland Data Layer.  Crop types are coded as 1-61, 66-77,
#' and 204-.  The user must provide the tif file acquired from
#' https://nassgeodata.gmu.edu/CropScape/.
#'
#' @param cropfile filename The full path name of a tif file of cropland data
#' @param SPDF SPDF the spatial data set to extract features from
#'
#' @return SPDF the same spatial data set as input but with cropland area features removed
#' @export
#'
#' @examples hms <- RemoveAgFires(hms, '../../GIS Data/CDL_2015_clip_20160407194453_1862402256.tif')
RemoveAgFires <- function(SPDF, cropfile) {
  # load the crop map
  crops <- raster::raster(cropfile)
  # Assign crop type
  ex <- raster::extract(crops, SPDF)
  SPDF@data$CropType <- ex
  # Crop types are 1-61, 66-77, 204-
  types <- read.csv('./SupportData/CropScape_2015_Stats.csv', stringsAsFactors = FALSE)
  SPDF@data <- left_join(SPDF@data, types, by = c('CropType'='Value'))
  SPDF[SPDF$AgLand==0,]
}

# Unify sf_id for this Tranche
make_sf_id <- function(SPDF, ds, Tranche) {
  pref <- paste0("T", as.character(Tranche), "_", as.character(ds))
  newSPDF <- sp::spChFIDs(SPDF, paste(pref, SPDF$sf_id, sep="_"))
}


# Here's a function to create a square polygon of user specified area around a centroid
# coords - data frame with longitude, latitude (in planar coordinates), and ID
# z - 1/2 length in meters of side of resulting square (pixel; e.g., 500 for 1 km2)
squarePolygon <- function(coords, z=500) {
  require(sp)
  x <- coords[1]
  y <- coords[2]
  id <- coords[3]
  poly <- c(x-z, y+z,
            x+z, y+z,
            x+z, y-z,
            x-z, y-z,
            x-z, y+z)
  m <- matrix(poly, 5, 2, byrow=TRUE)
  p <- Polygon(m)
  polys <- Polygons(list(p), id)
}

# Similar to above with slightly different interface that's easier to use with mapply
squarePolygonMulti <- function(x, y, id, z=500) {
  require(sp)
  poly <- c(x-z, y+z,
            x+z, y+z,
            x+z, y-z,
            x-z, y-z,
            x-z, y+z)
  m <- matrix(poly, 5, 2, byrow=TRUE)
  p <- Polygon(m)
  polys <- Polygons(list(p), id)
}

# Takes a SPDF of footprints with SF standard fields and returns clustered polygons
# with updated fields
clusterFootprints <- function(poly.data) {
  require(rgdal)
  require(rgeos)
  proj <- poly.data@proj4string@projargs

  # Dissolve and collect centroids (for merging attributes)
  dissolved <- gUnaryUnion(poly.data)
  centroids <- coordinates(poly.data)

  # just get the attributes we need (can clean this up)
  df <- poly.data@data
  points <- SpatialPointsDataFrame(coords=centroids, data=df, proj4string=CRS(proj), match.ID=FALSE)
  rm(poly.data)

  # split into multi
  # This takes the dissolved object (now a single poly) and splits it (like ArcMap explode)
  split <- lapply(dissolved@polygons, slot, "Polygons")
  # Now we need to get it back into SpatialPolygons object
  # found this mess after much searching
  # http://stackoverflow.com/questions/22936089/r-how-to-split-spatial-multipolygon-to-spatialpolygons-element?lq=1
  multi <- list()
  for(i in 1:length(split[1][[1]])){
    multi[i] <- Polygons(list(split[1][[1]][[i]]),i)
  }
  multi <- as(SpatialPolygons(multi), "SpatialPolygonsDataFrame")
  rm(split)

  # Add the projection info
  proj4string(multi) <- CRS(proj)

  # Populate some ids
  colnames(multi@data) <- c("sf_id")
  ids <- seq(from=1, to=length(multi$sf_id))
  multi$sf_id <- as.character(ids)

  # Intersect to get the aggregate values of all columns
  agg.min <- over(multi, points, fn=min, na.rm = TRUE)
  agg.max <- over(multi, points, fn=max, na.rm = TRUE)

  # Extract the area from the polygons
  area.m2 <- sapply(slot(multi, "polygons"),
                    function(x) sapply(slot(x, "Polygons"), slot, "area"))

  #Zip SF start and end dates and other fields into our dissolved polygons
  multi[["sf_area"]] <- (area.m2 * 0.0002471)
  multi[["sf_start"]] <- agg.min[["sf_start"]]
  multi[["sf_end"]] <- agg.max[["sf_end"]]
  multi[["sf_type"]] <- agg.min[["sf_type"]]
  multi[["sf_name"]] <- agg.min[["sf_name"]]
  multi[["sf_source"]] <- agg.min[["sf_source"]]

  return(multi)

}

# Convert a SpatialPolygonsDataFrame to a SpatialPointsDataFrame
ConvertPolyToPoint <- function(SPDF) {
  centroids <- coordinates(SPDF)
  df <- SPDF@data
  proj <- SPDF@proj4string@projargs
  points <- sp::SpatialPointsDataFrame(coords=centroids, data=df,
                                       proj4string=sp::CRS(proj), match.ID=FALSE)
}

# Frustratingly, native ifelse function makes dates lose their classes,
# see http://stackoverflow.com/questions/6668963/how-to-prevent-ifelse-from-turning-date-objects-into-numeric-objects
safe.if.else <- function(cond, yes, no) {
  structure(ifelse(cond, yes, no), class = class(yes))
}
