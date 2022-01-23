globalVariables(c(
  "coordinates", "estimatedElevation", "id2"
))

#' standardize and treat the BC PSP data
#'
#' @param dPath directory to download elevation data
#' @param filename2 the full filename of the output file
#' @param PSPplot the standardized PSP plot attribute data
#' @param PSPgis the standardized plot location sf object
#'
#' @return a text file
#'
#' @export
#' @importFrom data.table as.data.table setnames setcolorder fwrite
#' @importFrom raster extract
#' @importFrom sf as_Spatial
#' @importFrom sp coordinates
#' @importFrom reproducible prepInputs
#'

# run the rmd to get standardized PSP object
# elevation will produce a much better climate prediction, even if it isn't strictly necessary
# previous work showed that when elevation is missing,
# it is better to sample elevation from a DEM than to omit it
prepPSP_climateNA <- function(dPath, filename2, PSPplot, PSPgis) {
  # get an elevation DEM - this is the 7.5 arcsecond DEM used for fireSense, from GTOPO
  CanadaDEM <- prepInputs(
    url = "https://drive.google.com/file/d/121x_CfWy2XP_-1av0cYE7sxUfb4pmsup/view?usp=sharing",
    destinationPath = dPath
  )
  missingIDs <- PSPplot[is.na(Elevation), ]$OrigPlotID1
  missingElevation <- sf::as_Spatial(PSPgis[PSPgis$OrigPlotID1 %in% missingIDs, ])
  missingElevation <- data.table(
    OrigPlotID1 = missingElevation$OrigPlotID1,
    estimatedElevation = raster::extract(CanadaDEM, missingElevation)
  )
  if (anyNA(missingElevation$estimatedElevation)) {
    warning("Extracting elevation from DEM has failed for some plots")
  }

  PSPgis <- as_Spatial(PSPgis)
  PSPcoord <- as.data.table(coordinates(PSPgis))
  PSPcoord[, OrigPlotID1 := PSPgis$OrigPlotID1]

  PSPcoord <- PSPplot[, .(OrigPlotID1, Elevation)][PSPcoord, on = c("OrigPlotID1")]
  PSPcoord <- missingElevation[PSPcoord, on = c("OrigPlotID1")]
  PSPcoord[is.na(Elevation), Elevation := estimatedElevation]

  PSPcoord[, estimatedElevation := NULL]

  # prep for climateNA
  # the columns have to be called "id1", "id2", "lat", "long", "elev"
  # sampleData <- fread("C:/users/ieddy/Downloads/ClimateNA_v640/inputFiles/input_test.csv")
  setnames(
    PSPcoord, c("OrigPlotID1", "coords.x1", "coords.x2", "Elevation"),
    c("id1", "long", "lat", "elev")
  )
  PSPcoord[, id2 := ""]
  setcolorder(PSPcoord, neworder = c("id1", "id2", "lat", "long", "elev"))
  PSPcoord <- unique(PSPcoord)
  message("writing PSP locations data to csv for climateNA")
  fwrite(PSPcoord, file = filename2)
}
