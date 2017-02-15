#' InputTranche1
#'
#' Prepares generic polygon data for use in Tranche 1 (or elsewhere).  Formerly known as CleanPolygons.R
#' It does the following:
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
#' @examples InputTranche1('./config/MN_RX.R')
InputTranche1 <- function(config) {

  # Get the config info for this data source
  source(config, local = TRUE)

  # load shapefile, find and clean bad shapes
  shapes <- rgdal::readOGR(inpath, inname)
  good.geoms <- rgeos::gIsValid(shapes, byid=TRUE)
  if (any(good.geoms==FALSE)) {
    ### This doesn't seem to work anymore
    #shapes <- cleangeo::clgeo_Clean(shapes)

    ## Instead, employ the well known R/GEOS hack to deal with bad polygons
    #fix <- rgeos::gSimplify(shapes, tol = 0.00001)
    shapes <- rgeos::gBuffer(shapes, byid = TRUE, width = 0)
  }

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



#' InputGeomac
#'
#' Gets GeoMac data into shape for Tranche 1
#    1. Load data from shapefile
#    2. Look for intersecting polygons
#    3. Throw out all but the most recent
#    4. Write the first date
#    5. Add all SF fields
# #
#'
#'
#' @param config An SF config file
#'
#' @return SPDF
#' @export
#' @import dplyr
#'
#' @examples InputGeomac('./config/Geomac.R')
InputGeomac <- function(config) {

  # Load configuration
  source(config, local = TRUE)

  # load shapefile, find and clean bad shapes
  shapes <- rgdal::readOGR(inpath, inname)
  good.geoms <- rgeos::gIsValid(shapes, byid=TRUE)
  if (any(good.geoms==FALSE)) {
    ### This doesn't seem to work anymore
    #shapes <- cleangeo::clgeo_Clean(shapes)

    ## Instead, employ the well known R/GEOS hack to deal with bad polygons
    #fix <- rgeos::gSimplify(shapes, tol = 0.00001)
    shapes <- rgeos::gBuffer(shapes, byid = TRUE, width = 0)

  }

  # need to recalculate IDs to get code below to work (we need id == rownumber)
  n <- length(slot(shapes, 'polygons'))
  shapes <- sp::spChFIDs(shapes, as.character(1:n))

  # extract the data frame with IDs attached for convenience and add end.date
  # in proper date format
  d <- shapes@data %>%
    mutate(ID = as.numeric(rownames(as(shapes, "data.frame"))),
           end.date = as.Date(date_))

  # 1. find all names with more than one record
  conflicts <- group_by(d, fire_name) %>%
    summarise(records = n()) %>%
    filter(records > 1) %>%
    arrange(fire_name)

  # Look name by name and find most recent disjoint set (throw out old intersecting polys)
  # the dreaded for loop
  master.list <- list()
  master.df <- data.frame(ID=character(), first.date=character())
  for (f in 1:nrow(conflicts)) {
    firename <- conflicts$fire_name[[f]]
    set <- shapes[shapes$fire_name == firename,]
    # determine all intersections
    intersections <- rgeos::gIntersects(set, byid=TRUE)
    # for each intersection - select record with latest date (discard all others)
    # also record the first date for later use
    #### Step through each row of array - find the id with latest date among each true
    ### The final list will be the set of ids to discard
    # We'll start this as a loop and hopefully improve to array based later
    keep.list = list()
    first.date.list = list()
    for (n in 1:nrow(intersections)) {
      # These are the ids that intersect
      ids <- names(which(intersections[n,], useNames=TRUE))
      # Find the record with the best date
      set <- filter(d, ID %in% ids)
      keep.df <- arrange(set, desc(end.date)) %>%
        slice(1)
      # add the ID to the keep list
      keep.list <- c(keep.list, keep.df$ID)
      # Find the record with the first date
      first <- arrange(set, end.date) %>%
        slice(1)
      first.date.list <- c(first.date.list, as.character(first$end.date))
    }

    # merge first date and ids to a dataframe
    date.id <- data.frame(ID=as.numeric(unlist(keep.list)), first.date=unlist(first.date.list), stringsAsFactors=FALSE)

    # Get the unique values from the keep.list and add to master list
    date.id <- distinct(date.id)
    master.df <- rbind(master.df, date.id)

  }

  # add the single record files back to the master list
  single <- group_by(d, fire_name) %>%
    mutate(records = n(),
           first.date = as.character(end.date)) %>%
    filter(records == 1)

  single <- select(as.data.frame(single), ID, first.date)

  master.df <- rbind(master.df, single)

  # Make sure final master list is unique, convert to vector, then subset the shapefile
  master.df <- group_by(master.df, ID) %>%
    summarise(first.date = min(first.date)) %>%
    arrange(ID)
  master.vector <- unlist(master.df$ID)
  shapes.final <- shapes[master.vector,]

  # Perhaps will also add common fields here
  # Add ID as a key for joining
  shapes.final$ID <- as.character(row.names(shapes.final))
  master.df <- mutate(master.df, ID = as.character(ID))

#   # Add the first.date and everything else in master.df (see http://stackoverflow.com/questions/3650636/how-to-attach-a-simple-data-frame-to-a-spatialpolygondataframe-in-r)
#   shapes.final@data <- data.frame(shapes.final@data, master.df[match(shapes.final@data[,'ID'], master.df[,'ID']),])
  ## This doesn't seem to be working, try dplyr::innner_join
  shapes.final@data <- inner_join(shapes.final@data, master.df, by = 'ID')

    # Recalculate IDs (shouldn't need to do this)
  n <- length(slot(shapes.final, 'polygons'))
  shapes.final <- sp::spChFIDs(shapes.final, as.character(1:n))

  # Put into the central projection (CONUS Albers equal area)
  shapes.proj <- sp::spTransform(shapes.final, sp::CRS("+init=epsg:5070"))

  # Put in the standard sf fields
  shapes.proj@data <- mutate_(shapes.proj@data,
                              sf_area = AREA,
                              sf_end = END,
                              sf_type = TYPE,
                              sf_name = NAME,
                              sf_source = SOURCE,
                              sf_id = ID) %>%
    mutate(sf_start = as.Date(first.date))

  return(shapes.proj)

  #   # Export to shapefile
  #   writeOGR(shapes.proj, outpath, 'GeoMacProcessed', 'ESRI Shapefile')

}

