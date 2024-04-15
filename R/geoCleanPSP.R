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
#' @importFrom sf st_as_sf st_transform st_crs
#' @importFrom data.table set setDT
geoCleanPSP <- function(Locations) {
  setDT(Locations)

  if (is.null(Locations$Longitude)) {
    Locations$Longitude <- NA
    Locations$Latitude <- NA
  }

  if (is.null(Locations$Datum)) {
    Locations$Datum <- NA
  }

  # Seperate those using UTM
  LocationsUTM <- Locations[is.na(Longitude) | Longitude == 0, ]
  LocationsWGS <- Locations[!is.na(Longitude) & Longitude != 0, ]


  if (nrow(LocationsWGS) > 0) {
    LocationsWGS <- st_as_sf(
      x = LocationsWGS,
      coords = c("Longitude", "Latitude"),
      crs = st_crs(4326)
    )

    ToRemove <- c("Zone", "Datum", "Easting", "Northing", "Latitude", "Longitude")
    ToRemove <- ToRemove[ToRemove %in% colnames(LocationsWGS)]
    set(LocationsWGS, NULL, ToRemove, NULL)
  }

  if (nrow(LocationsUTM) > 0) {
    # reproject points to lat long
    ReprojFun <- function(UTMzone, points, datum = NULL) {
      oldCRS <- if (datum == "NAD83") {
        paste0(
          "+proj=utm +zone=", UTMzone, " +ellps=GRS80 +datum=",
          datum, " +units=m +no_defs "
        )
      } else if (datum == "NAD27") {
        st_crs(26717) # this is 2 points in Ontario with a particular UTM zone
      } else {
        stop("uncreognized datum in Location data")
      }
      output <- st_as_sf(
        x = points[points$Zone == UTMzone, ],
        coords = c("Easting", "Northing"),
        crs = oldCRS
      )
      newCRS <- st_crs(4326)
      output <- st_transform(output, newCRS) # reproject to longlat
      return(output)
    }

    LocationsNAD27 <- LocationsUTM[Datum == 27]
    if (nrow(LocationsNAD27) > 0) {
      # assume the others are NAD83 or WGS1984 which is trivially different at this scale
      LocationsUTM <- LocationsUTM[!OrigPlotID1 %in% LocationsNAD27]
    }

    LocationsReproj <- lapply(unique(LocationsUTM$Zone), ReprojFun,
      datum = "NAD83", points = LocationsUTM
    )

    if (nrow(LocationsNAD27) > 0) {
      LocationsReproj2 <- lapply(unique(LocationsNAD27$Zone),
        FUN = ReprojFun, points = LocationsNAD27, datum = "NAD27"
      )
      LocationsReproj <- append(LocationsReproj, LocationsReproj2)
      rm(LocationsReproj2)
    }

    LocationsReproj <- do.call(rbind, LocationsReproj)
    # this must happpen before rbind with the lat lon plots
    ToRemove <- c("Zone", "Datum", "Easting", "Northing", "Latitude", "Longitude")
    ToRemove <- ToRemove[ToRemove %in% colnames(LocationsReproj)]
    set(LocationsReproj, NULL, ToRemove, NULL)

    if (nrow(LocationsWGS) > 0) {
      Locations <- rbind(LocationsWGS, LocationsReproj)
    } else {
      Locations <- LocationsReproj
    }
  } else {
    Locations <- LocationsWGS
  }

  # Merge all datasets together
  if (is.null(Locations$Elevation)) {
    Locations$Elevation <- NA
  }


  Locations <- Locations[c("OrigPlotID1", "baseSA", "Elevation")]

  Locations <- Locations[!duplicated(Locations$OrigPlotID1), ] # drop repeat measures from GIS
  # Locations <- unique.data.frame(Locations[, "OrigPlotID1"])
  return(Locations)
}
