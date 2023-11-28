globalVariables(c(
  "Longitude"
))

#' convert plot location data to long/lat and output in a sf object
#'
#' @param Locations the compiled plot data
#'
#' @return an sf object with `OrigPlotID1` column
#'
#' @export
#' @importFrom sf st_as_sf st_transform
geoCleanPSP <- function(Locations) {

  # Seperate those using UTM
  LocationsUTM <- Locations[is.na(Longitude) | Longitude == 0, ]
  LocationsWGS <- Locations[!is.na(Longitude) & Longitude != 0, ]

  # a few points in UTM 11 are missing northing digits. Blame Alberta?
  LocationsUTM <- LocationsUTM[nchar(LocationsUTM$Northing) > 3, ] # better way to fix?
  LocationsWGS <- st_as_sf(
    x = LocationsWGS,
    coords = c("Longitude", "Latitude"),
    crs = "+proj=longlat +datum=WGS84"
  )
  set(LocationsWGS, NULL, c("Northing", "Easting"), NULL) # need equal number of columns

  LocationsReproj <- lapply(unique(LocationsUTM$Zone), FUN = function(x, points = LocationsUTM) {
    output <- st_as_sf(
      x = points[points$Zone == x, ],
      coords = c("Easting", "Northing"),
      crs = paste0("+proj=utm +zone=", x, " +ellps=GRS80 +datum=NAD83 +units=m +no_defs ")
    )
    set(output, NULL, c("Latitude", "Longitude"), NULL) # mostly NA or wrong
    output <- st_transform(output, crs = "+proj=longlat +datum=WGS84") # reproject to longlat
    return(output)
  })
  names(LocationsReproj) <- paste0("prev_UTMzone", unique(LocationsUTM$Zone))

  # Merge all datasets together
  LocationsReproj$WGS <- LocationsWGS
  LocationsReproj$deparse.level <- 1

  Locations <- do.call(rbind, args = LocationsReproj)

  # The dataset contains separate entries for different years at the same location, presumably for when CMI is sampled
  set(Locations, NULL, "Zone", NULL)
  Locations <- unique.data.frame(Locations[, "OrigPlotID1"])
  return(Locations)
}
