### sfUtils.R
#
#  Helper Functions for the SF algorithms
#

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

# Frustratingly, native ifelse function makes dates lose their classes,
# see http://stackoverflow.com/questions/6668963/how-to-prevent-ifelse-from-turning-date-objects-into-numeric-objects
safe.if.else <- function(cond, yes, no) {
  structure(ifelse(cond, yes, no), class = class(yes))
}
