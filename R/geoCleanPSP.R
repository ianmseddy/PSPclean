globalVariables(c(
  "Longitude"
))

#' convert plot location data to long/lat and output in a sf object
#'
#' @param Locations the compiled plot data
#'
#' @return an sf object with \code{OrigPlotID1} column
#'
#' @export
#' @importFrom sf st_as_sf st_transform
geoCleanPSP <- function(Locations) {

  if (is.null(Locations$Longitude)) {
    Locations$Longitude <- NA
    Locations$Latitude <- NA
  }
  # Seperate those using UTM
  LocationsUTM <- Locations[is.na(Longitude) | Longitude == 0, ]
  LocationsWGS <- Locations[!is.na(Longitude) & Longitude != 0, ]

  # a few points in UTM 11 are missing northing digits. Blame Alberta?
  LocationsUTM <- LocationsUTM[nchar(LocationsUTM$Northing) > 3, ] # better way to fix?

  if (nrow(LocationsWGS) > 0){
  LocationsWGS <- st_as_sf(
    x = LocationsWGS,
    coords = c("Longitude", "Latitude"),
    crs = "+proj=longlat +datum=WGS84"
  )
  set(LocationsWGS, NULL, c("Northing", "Easting"), NULL) # need equal number of columns
  }

  if (nrow(LocationsUTM) > 0) {
    #reproject points to lat long
    ReprojFun = function(UTMzone, points, datum = NULL) {

      oldCRS <- if (datum == "NAD83") {
        paste0("+proj=utm +zone=", UTMzone, " +ellps=GRS80 +datum=",
               datum, " +units=m +no_defs ")
      } else if (datum == "NAD27"){
        paste0("+init=epsg:4267 +proj=longlat +ellps=clrk66 +datum=NAD27 +no_defs + ",
               "nadgrids=@conus,@alaska,@ntv2_0.gsb,@ntv1_can.dat")
      } else {
        stop("uncreognized datum in Location data")
      }

      output <- st_as_sf(
        x = points[points$Zone == UTMzone, ],
        coords = c("Easting", "Northing"),
        crs = oldCRS)
      set(output, NULL, c("Latitude", "Longitude"), NULL) # mostly NA or wrong
      newCRS <- "+proj=longlat +datum=WGS84"
      output <- st_transform(output, newCRS) # reproject to longlat
      return(output)
    }

    if (!is.null(Locations$Datum)) {
      LocationsNAD27 <- LocationsUTM[Datum == 27] #this is only Ontario so far
      LocationsUTM <- LocationsUTM[!OrigPlotID1 %in% LocationsNAD27]
    } else {
      LocationsNAD27 <- LocationsUTM[0,]
    }

    LocationsReproj <- lapply(unique(LocationsUTM$Zone), ReprojFun,
                              datum = "NAD83", points = LocationsUTM)

    if (nrow(LocationsNAD27) > 0) {
      LocationsReproj2 <- lapply(unique(LocationsNAD27$Zone),
                                 FUN = ReprojFun, points = LocationsNAD27, datum = "NAD27")
      LocationsReproj <- c(LocationsReproj, LocationsReproj2)
      rm(LocationsReproj2)
    }
  }

  # Merge all datasets together
  if (nrow(LocationsWGS) > 0) {
    LocationsReproj <- do.call(rbind, LocationsReproj)
    #rbind does not have fill = TRUE
    LocationsWGS$Easting <- NULL
    LocationsWGS$Northing <- NULL
    Locations <- list(LocationsReproj, LocationsWGS)
    Locations <- do.call(rbind, Locations)
  } else {
    LocationsReproj <- do.call(rbind, LocationsReproj)
    Locations <- LocationsReproj
  }

  if (is.null(Locations$Elevation)) {
    Locations$Elevation <- NA
  }

  # The dataset contains separate entries for different years at the same location, presumably for when CMI is sampled
  Locations <- Locations[c("OrigPlotID1", "baseSA", "Elevation")]
  Locations <- Locations[!duplicated(Locations$OrigPlotID1),] #drop repeat measures from GIS
  # Locations <- unique.data.frame(Locations[, "OrigPlotID1"])
  return(Locations)
}
